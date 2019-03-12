module Hostable exposing (..)

import LocalStorage
import MeasureText
import Persist exposing (Persist, Export, User, Game, FollowCount)
import Persist.Encode
import Persist.Decode
import PortSocket
import Twitch.Helix.Decode as Helix exposing (Stream)
import Twitch.Helix as Helix
import Twitch.Kraken.Decode as Kraken
import Twitch.Kraken as Kraken
import Twitch.Tmi.Chat as Chat
--import Twitch.Tmi.ChatSamples as Chat
import TwitchId
import SelectCopy
import ScheduleGraph exposing (Event)
import View exposing (AppMode(..), ChannelStatus(..), AutoHostStatus(..))

import Browser
import Browser.Dom as Dom
import File
import File.Download
import Task
import Parser.Advanced as Parser
import Process
import Http
import Time exposing (Posix, Zone)
import Set
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query

requestLimit = 100
requestRate = 5
autoHostDelay = 10 * 60 * 1000
initialReconnectDelay = 1000
followExpiration = 90 * 24 * 60 * 60 * 1000
twitchIrc = "wss://irc-ws.chat.twitch.tv:443"
sampleHost = ":tmi.twitch.tv HOSTTARGET #wondibot :wondible 3\r\n"
sampleHostOff = ":tmi.twitch.tv HOSTTARGET #wondibot :- 0\r\n"

type Msg
  = Loaded (Maybe Persist)
  | Imported (Result Json.Decode.Error Export)
  | Self (Result Http.Error (List Helix.User))
  | Channel (Result Http.Error (List Helix.User))
  | Users (Result Http.Error (List Helix.User))
  | UserUpdate (Result Http.Error (List Helix.User))
  | UnknownUsers (Result Http.Error (List Helix.User))
  | HostingUser (Result Http.Error (List Helix.User))
  | Streams (Result Http.Error (List Stream))
  | ChannelStream (Result Http.Error (List Stream))
  | Games (Result Http.Error (List Helix.Game))
  | Videos String (Result Http.Error (List Helix.Video))
  | Followers String (Result Http.Error Int)
  | NextHost Posix
  | LivePoll Posix
  | Response Msg
  | NextRequest Posix
  | CurrentZone Zone
  | TextSize MeasureText.TextSize
  | Focused (Result Dom.Error ())
  | UI (View.Msg)
  | SocketEvent PortSocket.Id PortSocket.Event
  | Reconnect Posix

type ConnectionStatus
  = Disconnected
  | Rejected
  | Connect String Float
  | Connecting PortSocket.Id Float
  | Connected PortSocket.Id
  | LoggingIn PortSocket.Id String
  | LoggedIn PortSocket.Id String
  | Joining PortSocket.Id String String
  | Joined PortSocket.Id String String
  | Parting PortSocket.Id String String

type alias Model =
  { users : Dict String User
  , games : Dict String Game
  , scoredTags : Dict String Float
  , liveStreams : Dict String Stream
  , events : Dict String (List Event)
  , followers : Dict String FollowCount
  , auth : Maybe String
  , authLogin : Maybe String
  , autoChannel : Maybe String
  , ircConnection : ConnectionStatus
  , pendingUsers : List String
  , pendingUserStreams : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  , channelStatus : ChannelStatus
  , autoHostStatus : AutoHostStatus
  , previewVersion : Int
  , appMode : AppMode
  , selectedUser : Maybe String
  , selectedComment : Maybe (String, String)
  , addingComment : Maybe String
  , selectedGame : Maybe String
  , selectedTag : Maybe String
  , exportingAuth : Maybe String
  , location : Url
  , time : Posix
  , zone : Zone
  , labelWidths : Dict String Float
  }

main = Browser.document
  { init = init
  , update = updateWithChecks
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init href =
  let
    url = Url.fromString href
      |> Maybe.withDefault (Url Url.Http "" Nothing "" Nothing Nothing)
    auth = extractHashArgument "access_token" url
  in
  ( { users = Dict.empty
    , games = Dict.empty
    , scoredTags = Dict.empty
    , liveStreams = Dict.empty
    , events = Dict.empty
    , followers = Dict.empty
    , auth = auth
    , authLogin = Nothing
    , autoChannel = Nothing
    , ircConnection = Disconnected
    , pendingUsers = []
    , pendingUserStreams = []
    , pendingRequests = []
    , outstandingRequests = 0
    , channelStatus = Unknown
    , autoHostStatus = Incapable
    , previewVersion = 0
    , appMode = LiveStreams
    , selectedUser = Nothing
    , selectedComment = Nothing
    , addingComment = Nothing
    , selectedGame = Nothing
    , selectedTag = Nothing
    , exportingAuth = Nothing
    , location = url
    , time = Time.millisToPosix 0
    , zone = Time.utc
    , labelWidths = Dict.empty
    }
    --|> update (SocketEvent 0 (PortSocket.Message sampleHost)) |> Tuple.first
  , Cmd.batch 
    [ Task.perform CurrentZone Time.here
    , ScheduleGraph.allDays
      |> List.map ScheduleGraph.dayName
      |> List.map (\name -> MeasureText.getTextWidth {font = "1px sans-serif", text = name})
      |> Cmd.batch
    ]
  )

updateWithChecks msg model =
  let
    (m2,cmd2) = update msg model
    (m3,cmd3) = chatConnectionUpdate m2
  in
    (m3,Cmd.batch[cmd3, cmd2])

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( case mstate of
        Just state ->
          { model
          | users = state.users |> toUserDict
          , games = state.games |> toGameDict
          , scoredTags = state.scoredTags
          , events = state.events
          , auth = case state.auth of
            Just _ -> state.auth
            Nothing -> model.auth
          , autoChannel = case state.autoChannel of
            Just _ -> state.autoChannel
            Nothing -> model.authLogin
          , pendingUserStreams = List.map .id state.users
          }
        Nothing ->
          model
      )
      |> fetchNextUserBatch requestLimit
      |> fetchNextUserStreamBatch requestLimit
      |> fetchSelfIfAuth
      |> persist -- save auth if captured from url
    Imported (Ok imported) ->
      { model
      | users = imported.users |> toUserDict
      , games = imported.games
        |> List.foldl (\g games -> Dict.insert g.id g games) model.games
      , scoredTags = imported.scoredTags
      , liveStreams = Dict.empty
      , pendingUserStreams = List.map .id imported.users
      }
      |> fetchNextUserStreamBatch requestLimit
      |> persist
    Imported (Err err) ->
      (model, Cmd.none)
    Self (Ok (user::_)) ->
      ( { model
        | authLogin = Just user.login
        }
        |> appendRequests (case model.autoChannel of
          Just _ -> []
          Nothing -> [ fetchChannel user.login ]
        )
      , Cmd.none
      )
    Self (Ok _) ->
      let _ = Debug.log "self did not find user" "" in
      (model, Cmd.none)
    Self (Err error) ->
      let _ = Debug.log "self fetch error" error in
      (model, Cmd.none)
    Channel (Ok (user::_)) ->
      { model
      | autoChannel = Just user.login
      , channelStatus = Unknown
      }
      |> appendRequests [ fetchChannelStream user.login ]
      |> persist
    Channel (Ok _) ->
      let _ = Debug.log "channel did not find user" "" in
      (model, Cmd.none)
    Channel (Err error) ->
      let _=  Debug.log "channel fetch error" error in
      (model, Cmd.none)
    Users (Ok users) ->
      { model
      | users = addUsers (List.map (importUser>>persistUser) users) model.users
      , pendingUserStreams = List.append model.pendingUserStreams
        <| List.map .id users
      }
      |> fetchNextUserBatch requestLimit
      |> fetchNextUserStreamBatch requestLimit
      |> persist
    Users (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    UserUpdate (Ok users) ->
      { model
      | users = addUsers (List.map (importUser>>persistUser) users) model.users
      }
      |> persist
    UserUpdate (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    UnknownUsers (Ok users) ->
      ( { model
        | users = addUsers (List.map importUser users) model.users
        }
      , Cmd.none)
    UnknownUsers (Err error) ->
      let _ = Debug.log "unknown user fetch error" error in
      (model, Cmd.none)
    HostingUser (Ok (user::_)) ->
      ( { model
        | channelStatus = Hosting user.id
        }
      , Cmd.none
      )
    HostingUser (Ok _) ->
      let _ = Debug.log "hosting did not find user" "" in
      (model, Cmd.none)
    HostingUser (Err error) ->
      let _ = Debug.log "hosting fetch error" error in
      (model, Cmd.none)
    Streams (Ok streams) ->
      ( fetchNextGameBatch requestLimit
        <| fetchNextUserStreamBatch requestLimit
        <| fetchUnknownUsers streams
        <| fetchUnknownFollows streams
        <| fetchExpiredFollows streams
        { model
        | liveStreams = List.foldl (\s liveStreams ->
            Dict.insert s.userId s liveStreams
          ) model.liveStreams streams
        }
      , Cmd.none)
    Streams (Err error) ->
      let _= Debug.log "stream fetch error" error in
      (model, Cmd.none)
    ChannelStream (Ok (stream::_)) ->
      ( { model
        | channelStatus = Live
        , autoHostStatus = case model.autoHostStatus of
          Pending -> AutoEnabled
          _ -> model.autoHostStatus
        }
      , Cmd.none)
    ChannelStream (Ok []) ->
      ( { model | channelStatus = case model.channelStatus of
          Unknown -> Offline
          Offline -> Offline
          Hosting _ -> model.channelStatus
          Live -> Offline
        }
      , Cmd.none)
    ChannelStream (Err error) ->
      let _= Debug.log "channel stream fetch error" error in
      ( { model
        | channelStatus = Unknown
        }
      , Cmd.none)
    Games (Ok news) ->
      { model
      | games = List.foldl (\g games ->
          Dict.insert g.id (importGame g) games
        ) model.games news
      }
      |> persist
    Games (Err error) ->
      let _ = Debug.log "game fetch error" error in
      (model, Cmd.none)
    Videos userId (Ok videos) ->
      { model
      | events = Dict.insert userId
        (videos
          |> List.filter (\v -> v.videoType == Helix.Archive)
          |> List.map (\v -> {start = v.createdAt, duration = v.duration})
        ) model.events
      }
        |> persist
    Videos _ (Err error) ->
      let _ = Debug.log "video fetch error" error in
      (model, Cmd.none)
    Followers userId (Ok count) ->
      { model
      | followers = Dict.insert userId
          ({count = count, lastUpdated = model.time})
          model.followers
      }
        |> persist
    Followers _ (Err error) ->
      let _ = Debug.log "follower fetch error" error in
      (model, Cmd.none)
    NextHost time ->
      ( {model | autoHostStatus = Pending, time = time}
        |> appendRequests [model.autoChannel
          |> Maybe.map fetchChannelStream
          |> Maybe.withDefault Cmd.none]
        |> refreshUserStreams
        |> fetchNextUserStreamBatch requestLimit
      , Cmd.none
      )
    LivePoll time ->
      ( {model | time = time}
        |> appendRequests [model.autoChannel
          |> Maybe.map fetchChannelStream
          |> Maybe.withDefault Cmd.none]
      , Cmd.none
      )
    Response subMsg ->
      let
        (m2, cmd2) = update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
      in
      if m2.channelStatus == Offline
      && m2.autoHostStatus == Pending
      && m2.outstandingRequests == 0
      && List.isEmpty m2.pendingRequests then
        case View.sortedStreams m2 of
          top :: _ ->
            let
              muser = Dict.get top.userId m2.users
              name = muser |> Maybe.map .displayName |> Maybe.withDefault "unknown"
              _ = Debug.log "picking" name
            in
              ({m2 | autoHostStatus = AutoEnabled}, Cmd.batch [cmd2, hostChannel m2.ircConnection name])
          _ ->
            let _ = Debug.log "no streams" "" in
            ({m2 | autoHostStatus = AutoEnabled}, cmd2)
      else
        (m2, cmd2)
    NextRequest time ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            , time = time
            }, next)
        _ -> (model, Cmd.none)
    CurrentZone zone ->
      ( {model | zone = zone}, Cmd.none)
    TextSize {text, width} ->
      ( {model | labelWidths = Dict.insert text width model.labelWidths}, Cmd.none)
    Focused _ ->
      (model, Cmd.none)
    UI (View.Refresh) ->
      ( { model | previewVersion = model.previewVersion + 1}
        |> refreshUserStreams
        |> fetchNextUserStreamBatch requestLimit
      , Cmd.none)
    UI (View.Logout) ->
      { model | auth = Nothing, authLogin = Nothing }
        |> persist
    UI (View.Export) ->
      ( model
      , model
        |> export
        |> File.Download.string "hostable.json" "application/json"
      )
    UI (View.Import files) ->
      ( model
      , files
        |> List.map File.toString
        |> List.map (Task.perform receiveImported)
        |> Cmd.batch
      )
    UI (View.ExportAuth exporting) ->
      if exporting then
        ({model | exportingAuth = model.auth}, Cmd.none)
      else
        ({model | exportingAuth = Nothing}, Cmd.none)
    UI (View.CopyAuth controlId) ->
      (model, SelectCopy.selectCopy controlId)
    UI (View.ImportAuth token) ->
      { model | auth = Just token, authLogin = Nothing }
        |> fetchSelfIfAuth
        |> persist
    UI (View.AddChannel name) ->
      let lower = String.toLower name in
      if (List.filter
          (\u -> u.persisted && (String.toLower u.displayName) == lower)
          (Dict.values model.users)
        ) == [] then
        ( { model
          | pendingUsers = List.append model.pendingUsers [name]
          } |> fetchNextUserBatch requestLimit
        , Cmd.none
        )
      else
        (model, Cmd.none)
    UI (View.RemoveChannel userId) ->
      { model
      | users = Dict.update
          userId
          (\mu -> Maybe.map (\u -> {u|persisted = False}) mu)
          model.users
      , selectedUser = Nothing
      } |> persist
    UI (View.HostClicked userId controlId) ->
      ( { model
        | pendingRequests =
          List.append model.pendingRequests
            [ fetchUsersById [userId]
            , fetchVideos userId
            ]
        , selectedUser = Just userId
        }
      , SelectCopy.selectCopy controlId
      )
    UI (View.HostChannel target) ->
      ( model
      , hostChannel model.ircConnection target
      )
    UI (View.SelectComment userId comment) ->
      ( {model | selectedComment = Just (userId, comment)}, Cmd.none)
    UI (View.RemoveComment userId comment) ->
      { model
        | selectedComment = Nothing
        , users = removeComment userId comment model.users
      } |> persist
    UI (View.AddComment userId) ->
      ( { model | addingComment = Just userId }
      , Process.sleep 0
        |> Task.andThen (\_ -> Dom.focus "new-comment")
        |> Task.attempt Focused
      )
    UI (View.CreateComment userId comment) ->
      { model
        | addingComment = Nothing
        , users = addComment userId comment model.users
      } |> persist
    UI (View.SelectGame gameId) ->
      ( {model | selectedGame = Just gameId}
      , Process.sleep 100
        |> Task.andThen (\_ -> Dom.focus "edit-game-score")
        |> Task.attempt Focused
      )
    UI (View.UpdateGameScore gameId score) ->
      { model
        | selectedGame = Nothing
        , games = updateGameScore gameId score model.games
      } |> persist
    UI (View.SelectTag tag) ->
      ( {model | selectedTag = Just tag}
      , Process.sleep 100
        |> Task.andThen (\_ -> Dom.focus "edit-tag-score")
        |> Task.attempt Focused
      )
    UI (View.UpdateTagScore tag score) ->
      { model
        | selectedTag = Nothing
        , scoredTags = updateTagScore tag score model.scoredTags
      } |> persist
    UI (View.Navigate mode) ->
      ( {model | appMode = mode}, Cmd.none)
    UI (View.AutoHost enabled) ->
      ( {model | autoHostStatus = case (model.autoHostStatus, enabled) of
        (Incapable, _) -> Incapable
        (AutoDisabled, True) -> AutoEnabled
        (AutoDisabled, False) -> AutoDisabled
        (AutoEnabled, False) -> AutoDisabled
        (AutoEnabled, True) -> AutoEnabled
        (Pending, False) -> AutoDisabled
        (Pending, True) -> Pending
      }
      , Cmd.none
      )
    UI (View.HostOnChannel name) ->
      let lower = String.toLower name in
      ( model
        |> appendRequests [ fetchChannel lower ]
      , Cmd.none
      )
    SocketEvent id (PortSocket.Error value) ->
      let _ = Debug.log "websocket error" value in
      (model, Cmd.none)
    SocketEvent id (PortSocket.Connecting url) ->
      let _ = Debug.log "websocket connecting" id in
      ( { model | ircConnection = case model.ircConnection of
          Connect _ timeout -> Connecting id timeout
          Connecting _ timeout -> Connecting id timeout
          _ -> Connecting id initialReconnectDelay
        }
      , currentConnectionId model.ircConnection
          |> Maybe.map PortSocket.close
          |> Maybe.withDefault Cmd.none
      )
    SocketEvent id (PortSocket.Open url) ->
      let _ = Debug.log "websocket open" id in
      ( {model | ircConnection = Connected id}
      , Cmd.none
      )
    SocketEvent id (PortSocket.Close url) ->
      let _ = Debug.log "websocket closed" id in
      case model.ircConnection of
        Disconnected ->
          (model, Cmd.none)
        Rejected ->
          (model, Cmd.none)
        Connect _ timeout ->
          (model, Cmd.none)
        Connecting wasId timeout ->
          if id == wasId then
            ( { model
              | ircConnection = Connect twitchIrc timeout
              }
              , Cmd.none
            )
          else
            (model, Cmd.none)
        Connected wasId ->
          closeIfCurrent model wasId id
        LoggingIn wasId _ ->
          closeIfCurrent model wasId id
        LoggedIn wasId _ ->
          closeIfCurrent model wasId id
        Joining wasId _ _ ->
          closeIfCurrent model wasId id
        Joined wasId _ _ ->
          closeIfCurrent model wasId id
        Parting wasId _ _ ->
          closeIfCurrent model wasId id
    SocketEvent id (PortSocket.Message message) ->
      let _ = Debug.log "websocket message" message in
      case (Parser.run Chat.message message) of
        Ok lines ->
          List.foldl (reduce (chatResponse id message)) (model, Cmd.none) lines
        Err err ->
          let _ = Debug.log message err in
          (model, Cmd.none)
    Reconnect time ->
      case Debug.log "reconnect" model.ircConnection of
        Connect url timeout ->
          ( {model | ircConnection = Connect url (timeout*2)}
          , PortSocket.connect url
          )
        Connecting id timeout ->
          ( {model | ircConnection = Connect twitchIrc (timeout*2)}
          , Cmd.batch
            [ PortSocket.close id
            , PortSocket.connect twitchIrc
            ]
          )
        _ ->
          (model, Cmd.none)

chatResponse : PortSocket.Id -> String -> Chat.Line -> Model -> (Model, Cmd Msg)
chatResponse id message line model =
  let
    _ = line.tags
      |> List.map (\tag -> case tag of 
        Chat.UnknownTag _ _ ->
          Debug.log message tag
        _ -> tag
      )
  in
  case line.command of
    "CAP" -> (model, Cmd.none)
    "HOSTTARGET" ->
      let _ = Debug.log "hosttarget" line in
      case line.params of
        [_, target] ->
          case String.split " " target of
            "-"::_ ->
              ( { model
                | channelStatus = Unknown
                }
                |> appendRequests [model.autoChannel
                  |> Maybe.map fetchChannelStream
                  |> Maybe.withDefault Cmd.none]
              , Cmd.none
              )
            channel::_ ->
              (model |> appendRequests [fetchHostingUser channel], Cmd.none)
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)
    "JOIN" ->
      let
          user = line.prefix
            |> Maybe.map Chat.extractUserFromPrefix
            |> Maybe.withDefault (Err [])
            |> Result.withDefault "unknown"
          channel = line.params
            |> List.head
            |> Maybe.withDefault "unknown"
      in
      ( { model
        | ircConnection = Joined id user channel
        , autoHostStatus = AutoDisabled
        }
      , Cmd.none
      )
    "NOTICE" ->
      case line.params of
        ["*", "Improperly formatted auth"] ->
          let _ = Debug.log "Authentication failed" line.params in
          ({model | ircConnection = Rejected}, PortSocket.close id)
        _ -> 
          (model, Cmd.none)
    "PART" ->
      let
          user = line.prefix
            |> Maybe.map Chat.extractUserFromPrefix
            |> Maybe.withDefault (Err [])
            |> Result.withDefault "unknown"
      in
      ( { model
        | ircConnection = LoggedIn id user
        , channelStatus = case model.channelStatus of
          Hosting _ -> Offline
          _ -> model.channelStatus
        }
      , Cmd.none)
    "PING" -> 
      --let _ = Debug.log "PONG" "" in
      (model, PortSocket.send id ("PONG :tmi.twitch.tv"))
    "PRIVMSG" -> (model, Cmd.none)
    "ROOMSTATE" -> (model, Cmd.none)
    "USERNOTICE" ->
      let _ = Debug.log "usernotice" line in
      (model, Cmd.none)
    "001" -> (model, Cmd.none)
    "002" -> (model, Cmd.none)
    "003" -> (model, Cmd.none)
    "004" -> (model, Cmd.none)
    "353" -> (model, Cmd.none) --names list
    "366" -> -- end of names list
      let _ = Debug.log "joined room" (List.head (List.drop 1 line.params)) in
      (model, Cmd.none)
    "375" -> (model, Cmd.none)
    "372" -> (model, Cmd.none)
    "376" -> 
      --let _ = Debug.log "logged in" "" in
      ( {model | ircConnection = LoggedIn id (line.params |> List.head |> Maybe.withDefault "unknown")}
      , Cmd.batch
        [ PortSocket.send id "CAP REQ :twitch.tv/tags"
        , PortSocket.send id "CAP REQ :twitch.tv/commands"
        ]
      )
    _ ->
      let _ = Debug.log message line in
      (model, Cmd.none)


chatConnectionUpdate : Model -> (Model, Cmd Msg)
chatConnectionUpdate model =
  case (model.ircConnection, model.autoChannel) of
    (Disconnected, Just _) ->
      ( {model | ircConnection = Connect twitchIrc initialReconnectDelay}
      , Cmd.none
      )
    (Connected id, Just _) ->
      Maybe.map2 (\auth login ->
          ({model | ircConnection = LoggingIn id login}
          , Cmd.batch
            -- order is reversed, because elm feels like it
            [ PortSocket.send id ("NICK " ++ login)
            , PortSocket.send id ("PASS oauth:" ++ auth)
            ]
          )
        )
        model.auth
        model.authLogin
      |> Maybe.withDefault
        (model, Cmd.none)
    (LoggedIn id login, Just target) ->
      ( {model | ircConnection = Joining id login target}
      , PortSocket.send id ("JOIN #" ++ target)
      )
    (Joining id login channel, Nothing) ->
      ( {model | ircConnection = Disconnected}
      , PortSocket.close id
      )
    (Joined id login channel, Nothing) ->
      ( {model | ircConnection = Disconnected}
      , PortSocket.close id
      )
    (Joined id login channel, Just target) ->
      if channel /= ("#"++target) then
        ( {model | ircConnection = Parting id login channel}
        , PortSocket.send id ("part " ++ channel) )
      else
        (model, Cmd.none)
    (Parting id login channel, Nothing) ->
      ( {model | ircConnection = Disconnected}
      , PortSocket.close id
      )
    (_, _) ->
      (model, Cmd.none)

reduce : (msg -> Model -> (Model, Cmd Msg)) -> msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
reduce step msg (model, cmd) =
  let
    (m2, c2) = step msg model
  in
    (m2, Cmd.batch [cmd, c2])

hostChannel : ConnectionStatus -> String -> Cmd Msg
hostChannel ircConnection target =
  case ircConnection of
    Joined id user channel ->
      PortSocket.send id ("PRIVMSG " ++ channel ++ " :/host " ++ target)
    _ -> Cmd.none

closeIfCurrent : Model -> PortSocket.Id -> PortSocket.Id -> (Model, Cmd Msg)
closeIfCurrent model wasId id =
  if id == wasId then
    ( { model
      | ircConnection = Connect twitchIrc initialReconnectDelay
      , autoHostStatus = Incapable
      , channelStatus = case model.channelStatus of
        Hosting _ -> Offline
        _ -> model.channelStatus
      }
      , Cmd.none
    )
  else
    (model, Cmd.none)

currentConnectionId : ConnectionStatus -> Maybe PortSocket.Id
currentConnectionId connection =
  case connection of
    Disconnected ->
      Nothing
    Rejected ->
      Nothing
    Connect _ _ ->
      Nothing
    Connecting id _ ->
      Just id
    Connected id ->
      Just id
    LoggingIn id _ ->
      Just id
    LoggedIn id _ ->
      Just id
    Joining id _ _ ->
      Just id
    Joined id _ _ ->
      Just id
    Parting id _ _ ->
      Just id

toUserDict : List User -> Dict String User
toUserDict =
  List.map (\u -> (u.id, u)) >> Dict.fromList

toGameDict : List Game -> Dict String Game
toGameDict =
  List.map (\g -> (g.id, g)) >> Dict.fromList

addUsers : List User -> Dict String User -> Dict String User
addUsers news olds =
  let
    onlyNew = Dict.insert
    both = \userId new old users ->
      Dict.insert
        userId
        { old
        | displayName = new.displayName
        , persisted = old.persisted || new.persisted
        }
        users
    onlyOld = Dict.insert
  in
  Dict.merge
    onlyNew
    both
    onlyOld
    (news |> toUserDict)
    olds
    Dict.empty

removeComment : String -> String -> Dict String User -> Dict String User
removeComment userId comment users =
  Dict.update
    userId
    (Maybe.map (\u ->
      { u | tags = List.filter (\t -> t /= comment) u.tags }
    ))
    users

addComment : String -> String -> Dict String User -> Dict String User
addComment userId comment users =
  Dict.update
    userId
    (Maybe.map (\u ->
      { u | tags = comment :: u.tags }
    ))
    users

updateGameScore : String -> Float -> Dict String Game -> Dict String Game
updateGameScore gameId score games =
  Dict.update
    gameId
    (Maybe.map (\g ->
      { g | score =
          if score == 1.0 then
            Nothing
          else
            Just score
      }
    ))
    games

updateTagScore : String -> Float -> Dict String Float -> Dict String Float
updateTagScore tag score scoredTags =
  Dict.update
    tag
    (\_ ->
      if score == 1.0 then
        Nothing
      else
        Just score
    )
    scoredTags

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist
      (Dict.values model.users)
      (Dict.values model.games)
      model.scoredTags
      model.events
      model.followers
      model.auth
      model.autoChannel
    |> Persist.Encode.persist
    |> LocalStorage.saveJson

export : Model -> String
export model =
  Export
      (model.users |> Dict.values)
      (model.games |> Dict.values |> List.filter (\g -> g.score /= Nothing))
      model.scoredTags
    |> Persist.Encode.export
    |> Json.Encode.encode 2

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every (1000/requestRate) NextRequest
    , if model.autoHostStatus == AutoEnabled then
        case model.channelStatus of
          Unknown ->
            Time.every autoHostDelay NextHost
          Offline ->
            Time.every autoHostDelay NextHost
          Hosting _ ->
            Sub.none
          Live ->
            Time.every autoHostDelay LivePoll
      else
        Sub.none
    , LocalStorage.loadedJson Persist.Decode.persist Loaded
    , MeasureText.textSize TextSize
    , PortSocket.receive SocketEvent
    , case model.ircConnection of
        Connect _ timeout-> Time.every timeout Reconnect
        Connecting _ timeout-> Time.every timeout Reconnect
        _ -> Sub.none
    ]

importUser : Helix.User -> User
importUser user =
  { id = user.id
  , displayName = user.displayName
  , tags = []
  , persisted = False
  }

persistUser : User -> User
persistUser user =
  {user | persisted = True}

importGame : Helix.Game -> Game
importGame game =
  { id = game.id
  , name = game.name
  , boxArtUrl = game.boxArtUrl
  , score = Nothing
  }

commentsForStream : String -> List (String, List String) -> List String
commentsForStream userName users =
  List.filterMap (commentIfMatch userName) users
    |> List.head
    |> Maybe.withDefault []

commentIfMatch : String -> (String, List String) -> Maybe (List String)
commentIfMatch userName (name, comments) =
  if (String.toLower name) == (String.toLower userName) then
    Just comments
  else
    Nothing

receiveImported : String -> Msg
receiveImported string =
  string
    |> Json.Decode.decodeString Persist.Decode.export
    |> Result.mapError (Debug.log "import decode error")
    |> Imported

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | pendingUsers = List.drop batch model.pendingUsers
  } |> appendRequests [fetchUsersByLogin <| List.take batch model.pendingUsers]

fetchNextUserStreamBatch : Int -> Model -> Model
fetchNextUserStreamBatch batch model =
  { model
  | pendingUserStreams = List.drop batch model.pendingUserStreams
  } |> appendRequests [fetchStreamsByUserIds <| List.take batch model.pendingUserStreams]

refreshUserStreams : Model -> Model
refreshUserStreams model =
  { model
  | liveStreams = Dict.empty
  , pendingUserStreams = model.users
    |> Dict.filter (\_ {persisted} -> persisted == True)
    |> Dict.keys
  }

fetchNextGameBatch : Int -> Model -> Model
fetchNextGameBatch batch model =
  let known = Set.fromList <| Dict.keys model.games
      required = Set.fromList <| List.map .gameId <| Dict.values model.liveStreams
      missing = Set.toList <| Set.diff required known
  in
    model
    |> appendRequests [fetchGames <| List.take batch missing]

fetchUnknownUsers : List Stream -> Model -> Model
fetchUnknownUsers streams model =
  let known = Set.fromList <| Dict.keys model.users
      required = Set.fromList <| List.map .userId streams
      missing = Set.toList <| Set.diff required known
  in
    model
    |> appendRequests [fetchUnknownUsersById missing]

fetchUnknownFollows : List Stream -> Model -> Model
fetchUnknownFollows streams model =
  let known = Set.fromList <| Dict.keys model.followers
      required = Set.fromList <| List.map .userId streams
      missing = Set.toList <| Set.diff required known
  in
    model
    |> appendRequests (List.map fetchFollowers missing)

fetchExpiredFollows : List Stream -> Model -> Model
fetchExpiredFollows streams model =
  let
    now = Time.posixToMillis model.time
    required = Set.fromList <| List.map .userId streams
    expired = model.followers
      |> Dict.filter (\_ count -> (now - Time.posixToMillis count.lastUpdated) > followExpiration)
      |> Dict.keys
      |> Set.fromList
    needed = Set.toList <| Set.intersect required expired
  in
    model
    |> appendRequests (List.map fetchFollowers needed)

fetchSelfIfAuth :  Model -> Model
fetchSelfIfAuth model =
  case model.auth of
    Just _ ->
      model |> appendRequests [ fetchSelf model.auth ]
    Nothing ->
      model

appendRequests : List (Cmd Msg) -> Model -> Model
appendRequests cmds model = 
  { model
  | pendingRequests = List.append model.pendingRequests
    <| List.filter (\c -> c /= Cmd.none) cmds
  }

fetchUsersByLoginUrl : List String -> String
fetchUsersByLoginUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsersByLogin : List String -> Cmd Msg
fetchUsersByLogin users =
  if List.isEmpty users then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.users
      , tagger = Response << Users
      , url = (fetchUsersByLoginUrl users)
      }

fetchChannel : String -> Cmd Msg
fetchChannel user =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = Response << Channel
    , url = (fetchUsersByLoginUrl [user])
    }

fetchUsersByIdUrl : List String -> String
fetchUsersByIdUrl ids =
  "https://api.twitch.tv/helix/users?id=" ++ (String.join "&id=" ids)

fetchUsersById : List String -> Cmd Msg
fetchUsersById ids =
  if List.isEmpty ids then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.users
      , tagger = Response << UserUpdate
      , url = (fetchUsersByIdUrl ids)
      }

fetchUnknownUsersById : List String -> Cmd Msg
fetchUnknownUsersById ids =
  if List.isEmpty ids then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.users
      , tagger = Response << UnknownUsers
      , url = (fetchUsersByIdUrl ids)
      }

fetchHostingUser : String -> Cmd Msg
fetchHostingUser login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.users
    , tagger = Response << HostingUser
    , url = (fetchUsersByLoginUrl [login])
    }

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : Maybe String -> Cmd Msg
fetchSelf auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = Response << Self
    , url = fetchSelfUrl
    }

fetchStreamsByUserIdsUrl : List String -> String
fetchStreamsByUserIdsUrl userIds =
  "https://api.twitch.tv/helix/streams?type=live&first=100&user_id=" ++ (String.join "&user_id=" userIds)

fetchStreamsByUserIds : List String -> Cmd Msg
fetchStreamsByUserIds userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.streams
      , tagger = Response << Streams
      , url = (fetchStreamsByUserIdsUrl userIds)
      }

fetchChannelStreamUrl : String -> String
fetchChannelStreamUrl login =
  "https://api.twitch.tv/helix/streams?user_login=" ++ login

fetchChannelStream : String -> Cmd Msg
fetchChannelStream login =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.streams
    , tagger = Response << ChannelStream
    , url = (fetchChannelStreamUrl login)
    }

fetchGamesUrl : List String -> String
fetchGamesUrl gameIds =
  "https://api.twitch.tv/helix/games?id=" ++ (String.join "&id=" gameIds)

fetchGames : List String -> Cmd Msg
fetchGames gameIds =
  if List.isEmpty gameIds then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.games
      , tagger = Response << Games
      , url = (fetchGamesUrl gameIds)
      }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Response << (Videos userId)
    , url = (fetchVideosUrl userId)
    }

fetchFollowersUrl : String -> String
fetchFollowersUrl userId =
  "https://api.twitch.tv/helix/users/follows?first=1&to_id=" ++ userId

fetchFollowers : String -> Cmd Msg
fetchFollowers userId =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = followCount
    , tagger = Response << (Followers userId)
    , url = (fetchFollowersUrl userId)
    }

followCount : Json.Decode.Decoder Int
followCount =
    Json.Decode.field "total" Json.Decode.int

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing

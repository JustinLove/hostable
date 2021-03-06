module Hostable exposing (..)

import Decode exposing (Stream)
import LocalStorage
import Log
import MeasureText
import Persist exposing (Persist, Export, User, Game, FollowCount)
import Persist.Encode
import Persist.Decode
import PortSocket
import Twitch.Helix exposing (UserId)
import Twitch.Helix.Request as Helix
import Twitch.Tmi.Chat as Chat
--import Twitch.Tmi.ChatSamples as Chat
import TwitchId
import SelectCopy
import ScheduleGraph exposing (Event)
import View exposing (AppMode(..), ChannelStatus(..), AutoHostStatus(..), HostOnChannel(..))

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
import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query

requestLimit = 100
unauthenticatedRequestRate = 30
authenticatedRequestRate = 800
autoHostDelay = 10 * 60 * 1000
initialReconnectDelay = 1000
followExpiration = 90 * 24 * 60 * 60 * 1000
twitchIrc = "wss://irc-ws.chat.twitch.tv:443"
sampleHost = ":tmi.twitch.tv HOSTTARGET #wondibot :wondible 3\r\n"
sampleHostOff = ":tmi.twitch.tv HOSTTARGET #wondibot :- 0\r\n"
audioNoticeLength = 3 * 1000

type Msg
  = Loaded (Maybe Persist)
  | Imported (Result Decode.Error Export)
  | HttpError String Http.Error
  | Self (List String)
  | Channel (List String)
  | Users (List User)
  | UserUpdate (List User)
  | UnknownUsers (List User)
  | HostingUser (List User)
  | Streams (List Stream)
  | ChannelStream (List Stream)
  | Games (List Game)
  | Videos UserId (List Event)
  | Followers String Int
  | NextHost Posix
  | LivePoll Posix
  | AudioStart Posix
  | AudioEnd Posix
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
  , userPreviewVersion : Dict String Int
  , auth : Maybe String
  , authLogin : Maybe String
  , autoChannel : HostOnChannel
  , ircConnection : ConnectionStatus
  , pendingUsers : List String
  , pendingUserStreams : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  , channelStatus : ChannelStatus
  , autoHostStatus : AutoHostStatus
  , appMode : AppMode
  , selectedUser : Maybe String
  , selectedComment : Maybe (String, String)
  , addingComment : Maybe String
  , selectedGame : Maybe String
  , selectedTag : Maybe String
  , exportingAuth : Maybe String
  , audioNotice : Maybe Posix
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
    , userPreviewVersion = Dict.empty
    , auth = auth
    , authLogin = Nothing
    , autoChannel = case auth of
      Just _ -> HostOnLogin
      Nothing -> HostNothing
    , ircConnection = Disconnected
    , pendingUsers = []
    , pendingUserStreams = []
    , pendingRequests = []
    , outstandingRequests = 0
    , channelStatus = Unknown
    , autoHostStatus = Incapable
    , appMode = LiveStreams
    , selectedUser = Nothing
    , selectedComment = Nothing
    , addingComment = Nothing
    , selectedGame = Nothing
    , selectedTag = Nothing
    , exportingAuth = Nothing
    , audioNotice = Nothing
    , location = url
    , time = Time.millisToPosix 0
    , zone = Time.utc
    , labelWidths = Dict.empty
    }
    --|> update (SocketEvent 0 (PortSocket.Message sampleHost)) |> Tuple.first
    --|> update (AudioStart (Time.millisToPosix 0)) |> Tuple.first
  , Cmd.batch 
    [ Task.perform CurrentZone Time.here
    , ScheduleGraph.allDays
      |> List.map ScheduleGraph.dayName
      |> List.map (\name -> MeasureText.getTextWidth {font = "1px sans-serif", text = name})
      |> Cmd.batch
    ]
  )

logout : Model -> Model
logout model =
  { users = model.users
  , games = model.games
  , scoredTags = model.scoredTags
  , liveStreams = model.liveStreams
  , events = model.events
  , followers = model.followers
  , userPreviewVersion = model.userPreviewVersion
  , auth = Nothing
  , authLogin = Nothing
  , autoChannel = HostNothing
  , ircConnection = model.ircConnection -- let the state machine handle this
  , pendingUsers = []
  , pendingUserStreams = []
  , pendingRequests = []
  , outstandingRequests = model.outstandingRequests
  , channelStatus = Unknown
  , autoHostStatus = Incapable
  , appMode = model.appMode
  , selectedUser = Nothing
  , selectedComment = Nothing
  , addingComment = Nothing
  , selectedGame = Nothing
  , selectedTag = Nothing
  , exportingAuth = Nothing
  , audioNotice = Nothing
  , location = model.location
  , time = model.time
  , zone = model.zone
  , labelWidths = model.labelWidths
  }

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
          , followers = state.followers
          , auth = case state.auth of
            Just _ -> state.auth
            Nothing -> model.auth
          , autoChannel = case state.autoChannel of
            Just channel -> HostOn channel
            Nothing -> model.autoChannel
          , pendingUserStreams = List.map .id state.users
          }
        Nothing ->
          model
      )
      |> fetchNextUserBatch requestLimit
      |> fetchNextUserStreamBatch requestLimit
      |> appendUnauthenticatedRequests [ fetchSelf ]
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
      (model, Log.decodeError "import error: " err)
    HttpError source (Http.BadStatus 401) ->
      let m2 = logout model in
      ( m2
      , Cmd.batch
        [ saveState m2
        , Log.warn ("fetch auth error: " ++ source)
        ]
      )
    HttpError source (error) ->
      (model, Log.httpError ("fetch error: " ++ source) error)
    Self (login::_) ->
      ( { model
        | authLogin = Just login
        }
        |> appendUnauthenticatedRequests (case model.autoChannel of
          HostOnLogin -> [ fetchChannel login ]
          HostOn autoChannel-> [ fetchChannelStream autoChannel ]
          HostNothing -> []
        )
      , Cmd.none
      )
    Self _ ->
      (model, Log.warn "self did not find user")
    Channel (login::_) ->
      { model
      | autoChannel = HostOn login
      , channelStatus = Unknown
      }
      |> appendUnauthenticatedRequests [ fetchChannelStream login ]
      |> persist
    Channel _ ->
      (model, Log.warn "channel did not find user")
    Users users ->
      { model
      | users = addUsers (List.map persistUser users) model.users
      , pendingUserStreams = List.append model.pendingUserStreams
        <| List.map .id users
      }
      |> fetchNextUserBatch requestLimit
      |> fetchNextUserStreamBatch requestLimit
      |> persist
    UserUpdate users ->
      { model
      | users = addUsers (List.map persistUser users) model.users
      }
      |> persist
    UnknownUsers users ->
      ( { model
        | users = addUsers users model.users
        }
      , Cmd.none)
    HostingUser (user::_) ->
      ( { model
        | channelStatus = Hosting user.id
        }
      , Cmd.none
      )
    HostingUser _ ->
      (model, Log.warn "hosting did not find user")
    Streams streams ->
      ( fetchNextGameBatch requestLimit
        <| fetchNextUserStreamBatch requestLimit
        <| fetchUnknownUsers streams <| fetchUnknownFollows streams
        <| fetchExpiredFollows streams
        { model
        | liveStreams = List.foldl (\s liveStreams ->
            Dict.insert s.userId s liveStreams
          ) model.liveStreams streams
        , users = List.foldl (\s users ->
            Dict.update s.userId (Maybe.map (\user -> {user | displayName = s.userName})) users
          ) model.users streams
        }
      , Cmd.none)
    ChannelStream (stream::_) ->
      ( { model
        | channelStatus = Live
        , autoHostStatus = case model.autoHostStatus of
          Pending -> AutoEnabled
          _ -> model.autoHostStatus
        }
      , Cmd.none)
    ChannelStream [] ->
      ( { model | channelStatus = case model.channelStatus of
          Unknown -> Offline
          Offline -> Offline
          Hosting _ -> model.channelStatus
          Live -> Offline
        }
      , Cmd.none)
    Games news ->
      { model
      | games = List.foldl (\g games ->
          Dict.insert g.id g games
        ) model.games news
      }
      |> persist
    Videos userId events ->
      { model
      | events = Dict.insert userId events model.events
      }
        |> persist
    Followers userId count ->
      { model
      | followers = Dict.insert userId
          ({count = count, lastUpdated = model.time})
          model.followers
      }
        |> persist
    NextHost time ->
      ( {model | autoHostStatus = Pending, time = time}
        |> pollAutoChannel
        |> refreshUserStreams
        |> fetchNextUserStreamBatch requestLimit
      , Cmd.none
      )
    LivePoll time ->
      ( {model | time = time}
        |> pollAutoChannel
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
            in
              ( {m2 | autoHostStatus = AutoEnabled}
              , Cmd.batch
                [ cmd2
                , hostChannel m2.ircConnection name
                , Log.info ("picking " ++ name)
                ])
          _ ->
            ( {m2 | autoHostStatus = AutoEnabled}
            , Cmd.batch
              [ cmd2
              , Log.info "no streams"
              ])
      else
        (m2, cmd2)
    AudioStart time ->
      ({model | audioNotice = Just time}, Cmd.none)
    AudioEnd _ ->
      ({model | audioNotice = Nothing}, Cmd.none)
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
      ( model
        |> refreshUserStreams
        |> fetchNextUserStreamBatch requestLimit
      , Cmd.none)
    UI (View.Logout) ->
      logout model
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
        |> appendRequests [ fetchSelf token ]
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
        | selectedUser = Just userId
        , userPreviewVersion = Dict.update userId (\version ->
          case version of
            Just n -> Just (n + 1)
            Nothing -> Just 1
          )
          model.userPreviewVersion
        }
          |> appendUnauthenticatedRequests
              [ fetchUsersById [userId]
              , fetchVideos userId
              ]
      , SelectCopy.selectCopy controlId
      )
    UI (View.HostChannel target) ->
      ( model
      , hostChannel model.ircConnection target
      )
    UI (View.RaidChannel target) ->
      ( model
      , raidChannel model.ircConnection target
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
        |> appendUnauthenticatedRequests [ fetchChannel lower ]
      , Cmd.none
      )
    UI View.RemoveHostTracking ->
      { model
      | autoChannel = HostNothing
      , channelStatus = Unknown
      }
      |> persist
    SocketEvent id (PortSocket.Error value) ->
      (model, Log.error "websocket error" value)
    SocketEvent id (PortSocket.Connecting url) ->
      --let _ = Debug.log "websocket connecting" id in
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
      --let _ = Debug.log "websocket open" id in
      ( {model | ircConnection = Connected id}
      , Cmd.none
      )
    SocketEvent id (PortSocket.Close url) ->
      --let _ = Debug.log "websocket closed" id in
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
      --let _ = Debug.log "websocket message" message in
      case (Parser.run Chat.message message) of
        Ok lines ->
          List.foldl (reduce (chatResponse id message)) (model, Cmd.none) lines
        Err err ->
          (model, parseError message err)
    Reconnect time ->
      case model.ircConnection of
        Connect url timeout ->
          ( {model | ircConnection = Connect url (timeout*2)}
          , Cmd.batch
            [ PortSocket.connect url
            , Log.info "reconnect"
            ]
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
  reduce (chatCommandResponse id message) line (model, collectUnknownTags message line)

collectUnknownTags : String -> Chat.Line -> Cmd Msg
collectUnknownTags message line =
  line.tags
    |> List.filterMap (\tag -> case tag of 
      Chat.UnknownTag key value ->
        Cmd.batch
          [ ("unknown tag" ++ key ++ "=" ++ value)
            |> Log.info
          , Log.info message
          ]
          |> Just
      _ -> Nothing
    )
    |> Cmd.batch

chatCommandResponse : PortSocket.Id -> String -> Chat.Line -> Model -> (Model, Cmd Msg)
chatCommandResponse id message line model =
  case line.command of
    "CAP" -> (model, Cmd.none)
    "HOSTTARGET" ->
      let log = ("hosttarget" :: line.params) |> String.join " " |> Log.info in
      case line.params of
        [_, target] ->
          case String.split " " target of
            "-"::_ ->
              ( { model
                | channelStatus = Unknown
                }
                |> pollAutoChannel
              , Cmd.batch
                [ (Time.now |> Task.perform AudioStart)
                , log
                ]
              )
            channel::_ ->
              (model |> appendUnauthenticatedRequests [fetchHostingUser channel]
              , Cmd.none)
            _ -> (model, log)
        _ -> (model, log)
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
          ( {model | ircConnection = Rejected}
          , Cmd.batch
            [ PortSocket.close id
            , Log.error "Authentication failed" (encodeLine line)
            ]
          )
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
      --let _ = Debug.log "usernotice" line in
      (model, Cmd.none)
    "USERSTATE" ->
      --let _ = Debug.log "userstate" line in
      (model, Cmd.none)
    "001" -> (model, Cmd.none)
    "002" -> (model, Cmd.none)
    "003" -> (model, Cmd.none)
    "004" -> (model, Cmd.none)
    "353" -> (model, Cmd.none) --names list
    "366" -> -- end of names list
      ( model
      , line.params
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault "unknown"
        |> (\s -> "joined room " ++ s)
        |> Log.info
      )
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
      (model, unknownMessage message line)

chatConnectionUpdate : Model -> (Model, Cmd Msg)
chatConnectionUpdate model =
  let
    autoChannel = case model.autoChannel of
      HostOn channel -> Just channel
      HostOnLogin -> Nothing
      HostNothing -> Nothing
  in
  case (model.ircConnection, autoChannel) of
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

raidChannel : ConnectionStatus -> String -> Cmd Msg
raidChannel ircConnection target =
  case ircConnection of
    Joined id user channel ->
      PortSocket.send id ("PRIVMSG " ++ channel ++ " :/raid " ++ target)
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
      (case model.autoChannel of
        HostOn channel -> Just channel
        HostOnLogin -> Nothing
        HostNothing -> Nothing
      )
    |> Persist.Encode.persist
    |> LocalStorage.saveJson

export : Model -> String
export model =
  Export
      (model.users |> Dict.values)
      (model.games |> Dict.values |> List.filter (\g -> g.score /= Nothing))
      model.scoredTags
    |> Persist.Encode.export
    |> Encode.encode 2

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        case model.auth of
          Just _ ->
            Time.every (1000*60*1.05/authenticatedRequestRate) NextRequest
          Nothing ->
            Time.every (1000*60*1.05/unauthenticatedRequestRate) NextRequest
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
    , model.audioNotice
      |> Maybe.map (\_ -> Time.every audioNoticeLength AudioEnd)
      |> Maybe.withDefault Sub.none
    ]

persistUser : User -> User
persistUser user =
  {user | persisted = True}

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
    |> Decode.decodeString Persist.Decode.export
    --|> Result.mapError (Debug.log "import decode error")
    |> Imported

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | pendingUsers = List.drop batch model.pendingUsers
  } |> appendUnauthenticatedRequests
    [fetchUsersByLogin <| List.take batch model.pendingUsers]

fetchNextUserStreamBatch : Int -> Model -> Model
fetchNextUserStreamBatch batch model =
  { model
  | pendingUserStreams = List.drop batch model.pendingUserStreams
  } |> appendUnauthenticatedRequests
      [fetchStreamsByUserIds <| List.take batch model.pendingUserStreams]

pollAutoChannel : Model -> Model
pollAutoChannel model =
  --let _ = Debug.log "poll" model.autoChannel in
  model
    |> appendUnauthenticatedRequests (case model.autoChannel of
      HostOn autoChannel -> [ fetchChannelStream autoChannel ]
      _ -> []
    )

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
    |> appendUnauthenticatedRequests
      [fetchGames <| List.take batch missing]

fetchUnknownUsers : List Stream -> Model -> Model
fetchUnknownUsers streams model =
  let known = Set.fromList <| Dict.keys model.users
      required = Set.fromList <| List.map .userId streams
      missing = Set.toList <| Set.diff required known
  in
    model
    |> appendUnauthenticatedRequests
      [fetchUnknownUsersById missing]

fetchUnknownFollows : List Stream -> Model -> Model
fetchUnknownFollows streams model =
  let known = Set.fromList <| Dict.keys model.followers
      required = Set.fromList <| List.map .userId streams
      missing = Set.toList <| Set.diff required known
  in
    model
    |> appendUnauthenticatedRequests
      (List.map fetchFollowers missing)

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
    |> appendUnauthenticatedRequests
      (List.map fetchFollowers needed)

appendRequests : List (Cmd Msg) -> Model -> Model
appendRequests cmds model = 
  { model
  | pendingRequests = cmds
    |> List.filter (\c -> c /= Cmd.none)
    |> List.append model.pendingRequests
  }

appendUnauthenticatedRequests : List (String -> Cmd Msg) -> Model -> Model
appendUnauthenticatedRequests reqs model = 
  model |> appendRequests (case model.auth of
      Just auth -> List.map (\r -> r auth) reqs
      Nothing -> []
    )

httpResponse : String -> (a -> Msg)-> Result Http.Error a -> Msg
httpResponse source success result =
  case result of
    Ok value -> success value |> Response
    Err err -> HttpError source err |> Response

fetchUsersByLoginUrl : List String -> String
fetchUsersByLoginUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsersByLogin : List String -> String -> Cmd Msg
fetchUsersByLogin users auth =
  if List.isEmpty users then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Decode.users
      , tagger = httpResponse "Users" Users
      , url = (fetchUsersByLoginUrl users)
      }

fetchChannel : String -> String -> Cmd Msg
fetchChannel user auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Decode.logins
    , tagger = httpResponse "Channel" Channel
    , url = (fetchUsersByLoginUrl [user])
    }

fetchUsersByIdUrl : List String -> String
fetchUsersByIdUrl ids =
  "https://api.twitch.tv/helix/users?id=" ++ (String.join "&id=" ids)

fetchUsersById : List String -> String -> Cmd Msg
fetchUsersById ids auth =
  if List.isEmpty ids then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Decode.users
      , tagger = httpResponse "UserUpdate" UserUpdate
      , url = (fetchUsersByIdUrl ids)
      }

fetchUnknownUsersById : List String -> String -> Cmd Msg
fetchUnknownUsersById ids auth =
  if List.isEmpty ids then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Decode.users
      , tagger = httpResponse "UnknownUsers" UnknownUsers
      , url = (fetchUsersByIdUrl ids)
      }

fetchHostingUser : String -> String -> Cmd Msg
fetchHostingUser login auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Decode.users
    , tagger = httpResponse "HostingUser" HostingUser
    , url = (fetchUsersByLoginUrl [login])
    }

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : String -> Cmd Msg
fetchSelf auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Decode.logins
    , tagger = httpResponse "Self" Self
    , url = fetchSelfUrl
    }

fetchStreamsByUserIdsUrl : List String -> String
fetchStreamsByUserIdsUrl userIds =
  "https://api.twitch.tv/helix/streams?type=live&first=100&user_id=" ++ (String.join "&user_id=" userIds)

fetchStreamsByUserIds : List String -> String -> Cmd Msg
fetchStreamsByUserIds userIds auth =
  if List.isEmpty userIds then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Decode.streams
      , tagger = httpResponse "Streams" Streams
      , url = (fetchStreamsByUserIdsUrl userIds)
      }

fetchChannelStreamUrl : String -> String
fetchChannelStreamUrl login =
  "https://api.twitch.tv/helix/streams?user_login=" ++ login

fetchChannelStream : String -> String -> Cmd Msg
fetchChannelStream login auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Decode.streams
    , tagger = httpResponse "ChannelStream" ChannelStream
    , url = (fetchChannelStreamUrl login)
    }

fetchGamesUrl : List String -> String
fetchGamesUrl gameIds =
  "https://api.twitch.tv/helix/games?id=" ++ (String.join "&id=" gameIds)

fetchGames : List String -> String -> Cmd Msg
fetchGames gameIds auth =
  if List.isEmpty gameIds then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Decode.games
      , tagger = httpResponse "Games" Games
      , url = (fetchGamesUrl gameIds)
      }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&type=archive&user_id=" ++ userId

fetchVideos : String -> String -> Cmd Msg
fetchVideos userId auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Decode.events
    , tagger = httpResponse "Videos" (Videos userId)
    , url = (fetchVideosUrl userId)
    }

fetchFollowersUrl : String -> String
fetchFollowersUrl userId =
  "https://api.twitch.tv/helix/users/follows?first=1&to_id=" ++ userId

fetchFollowers : String -> String -> Cmd Msg
fetchFollowers userId auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = followCount
    , tagger = httpResponse "Followers" (Followers userId)
    , url = (fetchFollowersUrl userId)
    }

followCount : Decode.Decoder Int
followCount =
    Decode.field "total" Decode.int

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing

parseError message error = Log.error message (Encode.string (Chat.deadEndsToString error))

unknownMessage : String -> Chat.Line -> Cmd Msg
unknownMessage message line =
  Log.debug ("unknown: " ++ message) (encodeLine line)

encodeLine : Chat.Line -> Encode.Value
encodeLine line =
  Encode.object
    [ ("prefix", line.prefix |> Maybe.map Encode.string |> Maybe.withDefault Encode.null)
    , ("command", Encode.string line.command)
    , ("params", Encode.list Encode.string line.params)
    ]

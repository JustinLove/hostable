module Hostable exposing (..)

import Decode exposing (Stream)
import LocalStorage
import Log
import MeasureText
import Persist exposing (Persist, Export, User, Game, FollowCount)
import Persist.Encode
import Persist.Decode
import Twitch.Helix exposing (UserId)
import Twitch.Helix.Request as Helix
import TwitchId
import SelectCopy
import ScheduleGraph exposing (Event)
import View exposing (AppMode(..))

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
followExpiration = 90 * 24 * 60 * 60 * 1000

type Msg
  = Loaded (Maybe Persist)
  | Imported (Result Decode.Error Export)
  | HttpError String Http.Error
  | Self (List String)
  | Users (List User)
  | UserUpdate (List User)
  | UnknownUsers (List User)
  | Streams (List Stream)
  | Games (List Game)
  | Videos UserId (List Event)
  | Followers String Int
  | Response Msg
  | NextRequest Posix
  | CurrentZone Zone
  | TextSize MeasureText.TextSize
  | Focused (Result Dom.Error ())
  | UI (View.Msg)

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
  , pendingUsers : List String
  , pendingUserStreams : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
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
  , update = update
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
    , pendingUsers = []
    , pendingUserStreams = []
    , pendingRequests = []
    , outstandingRequests = 0
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
  , pendingUsers = []
  , pendingUserStreams = []
  , pendingRequests = []
  , outstandingRequests = model.outstandingRequests
  , appMode = model.appMode
  , selectedUser = Nothing
  , selectedComment = Nothing
  , addingComment = Nothing
  , selectedGame = Nothing
  , selectedTag = Nothing
  , exportingAuth = Nothing
  , location = model.location
  , time = model.time
  , zone = model.zone
  , labelWidths = model.labelWidths
  }

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
      , Cmd.none
      )
    Self _ ->
      (model, Log.warn "self did not find user")
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
    Response subMsg ->
      update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
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

reduce : (msg -> Model -> (Model, Cmd Msg)) -> msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
reduce step msg (model, cmd) =
  let
    (m2, c2) = step msg model
  in
    (m2, Cmd.batch [cmd, c2])

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
      Nothing -- autoChannel
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
    , LocalStorage.loadedJson Persist.Decode.persist Loaded
    , MeasureText.textSize TextSize
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
  "https://api.twitch.tv/helix/channels/followers?first=1&broadcaster_id=" ++ userId

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

module Hostable exposing (..)

import LocalStorage
import MeasureText
import Persist exposing (Persist, Export, User, Game)
import Persist.Encode
import Persist.Decode
import Twitch.Helix.Decode as Helix exposing (Stream)
import Twitch.Helix as Helix
import Twitch.Kraken.Decode as Kraken
import Twitch.Kraken as Kraken
import TwitchId
import ScheduleGraph exposing (Event)
import View
import Harbor

import Browser
import Browser.Dom as Dom
import Task
import Process
import Http
import Time exposing (Posix, Zone)
import Set
import Dict exposing (Dict)
import Json.Decode
import Json.Encode

requestLimit = 100
requestRate = 5

type Msg
  = Loaded (Maybe Persist)
  | Imported (Result Json.Decode.Error Export)
  | Users (Result Http.Error (List Helix.User))
  | UserUpdate (Result Http.Error (List Helix.User))
  | UnknownUsers (Result Http.Error (List Helix.User))
  | Streams (Result Http.Error (List Stream))
  | Games (Result Http.Error (List Helix.Game))
  | Videos String (Result Http.Error (List Helix.Video))
  | Response Msg
  | NextRequest Posix
  | CurrentZone Zone
  | TextSize MeasureText.TextSize
  | Focused (Result Dom.Error ())
  | UI (View.Msg)

type alias Model =
  { users : Dict String User
  , games : Dict String Game
  , liveStreams : Dict String Stream
  , events : Dict String (List Event)
  , pendingUsers : List String
  , pendingUserStreams : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  , previewVersion : Int
  , selectedUser : Maybe String
  , selectedComment : Maybe (String, String)
  , addingComment : Maybe String
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

init : () -> (Model, Cmd Msg)
init _ =
  ( { users = Dict.empty
    , games = Dict.empty
    , liveStreams = Dict.empty
    , events = Dict.empty
    , pendingUsers = []
    , pendingUserStreams = []
    , pendingRequests = []
    , outstandingRequests = 0
    , previewVersion = 0
    , selectedUser = Nothing
    , selectedComment = Nothing
    , addingComment = Nothing
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

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( ( case mstate of
          Just state ->
            { model
            | users = state.users |> toUserDict
            , games = state.games |> toGameDict
            , events = state.events
            , pendingUserStreams = List.map .id state.users
            }
          Nothing ->
            model
        )
        |> fetchNextUserBatch requestLimit
        |> fetchNextUserStreamBatch requestLimit
      , Cmd.none)
    Imported (Ok imported) ->
      { model
      | users = imported.users |> toUserDict
      , liveStreams = Dict.empty
      , pendingUserStreams = List.map .id imported.users
      }
      |> fetchNextUserStreamBatch requestLimit
      |> persist
    Imported (Err err) ->
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
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    UserUpdate (Ok users) ->
      { model
      | users = addUsers (List.map (importUser>>persistUser) users) model.users
      }
      |> persist
    UserUpdate (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    UnknownUsers (Ok users) ->
      ( { model
        | users = addUsers (List.map importUser users) model.users
        }
      , Cmd.none)
    UnknownUsers (Err error) ->
      { e = Debug.log "unknown user fetch error" error
      , r = (model, Cmd.none)}.r
    Streams (Ok streams) ->
      ( fetchNextGameBatch requestLimit
        <| fetchNextUserStreamBatch requestLimit
        <| fetchUnknownUsers streams
        { model
        | liveStreams = List.foldl (\s liveStreams ->
            Dict.insert s.userId s liveStreams
          ) model.liveStreams streams
        }
      , Cmd.none)
    Streams (Err error) ->
      { e = Debug.log "stream fetch error" error
      , r = (model, Cmd.none)}.r
    Games (Ok news) ->
      { model
      | games = List.foldl (\g games ->
          Dict.insert g.id g games
        ) model.games news
      }
      |> persist
    Games (Err error) ->
      { e = Debug.log "game fetch error" error
      , r = (model, Cmd.none)}.r
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
      (fetchNextUserStreamBatch requestLimit
        { model
        | liveStreams = Dict.empty
        , pendingUserStreams = model.users
          |> Dict.filter (\_ {persisted} -> persisted == True)
          |> Dict.keys
        , previewVersion = model.previewVersion + 1
        }
      , Cmd.none)
    UI (View.Import files) ->
      (model, Harbor.read files)
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
      , Harbor.select controlId
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

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist
      (Dict.values model.users)
      (Dict.values model.games)
      model.events
    |> Persist.Encode.persist
    |> LocalStorage.saveJson

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every (1000/requestRate) NextRequest
    , LocalStorage.loadedJson Persist.Decode.persist Loaded
    , Harbor.fileContents receiveImported
    , MeasureText.textSize TextSize
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
  "https://api.twitch.tv/helix/videos?first=100&user_id=" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Helix.videos
    , tagger = Response << (Videos userId)
    , url = (fetchVideosUrl userId)
    }

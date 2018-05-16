module Hostable exposing (..)

import Persist exposing (Persist, User, Game)
import Persist.Encode exposing (persist)
import Persist.Decode exposing (persist)
import Twitch.Deserialize exposing (LiveStream)
import Twitch exposing (helix)
import TwitchId
import UserList
import ScheduleGraph exposing (Event)
import View
import Harbor

import Html
import Http
import Time exposing (Time)
import Set
import Dict exposing (Dict)
import Json.Decode
import Json.Encode

requestLimit = 100
requestRate = 5

type Msg
  = Loaded (Maybe Persist)
  | Users (Result Http.Error (List Twitch.Deserialize.User))
  | Streams (Result Http.Error (List LiveStream))
  | Games (Result Http.Error (List Twitch.Deserialize.Game))
  | Videos String (Result Http.Error (List Twitch.Deserialize.Video))
  | Response Msg
  | NextRequest Time
  | UI (View.Msg)

type alias Model =
  { users : List User
  , games : List Game
  , liveStreams : List LiveStream
  , events : Dict String (List Event)
  , missingUsers : List String
  , pendingUsers : List String
  , pendingStreams : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  , previewVersion : Int
  , time : Time
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : (Model, Cmd Msg)
init =
  ( { users = []
    , games = []
    , liveStreams = []
    , events = Dict.empty
    , missingUsers = []
    , pendingUsers = []
    , pendingStreams = []
    , pendingRequests = []
    , outstandingRequests = 0
    , previewVersion = 0
    , time = 0
    }
  , Cmd.none
  )

desiredUserNames : Set.Set String
desiredUserNames = Set.fromList <| List.map (Tuple.first >> String.toLower) UserList.users

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( ( case mstate of
          Just state ->
            { model
            | users = state.users
            , games = state.games
            }
          Nothing ->
            model
        )
        |> resolveLoadedState
        |> fetchNextUserBatch requestLimit
        |> fetchNextStreamBatch requestLimit
      , Cmd.none)
    Users (Ok users) ->
      { model
      | users = List.append model.users <| List.map importUser users
      , pendingStreams = List.append model.pendingStreams
        <| List.map .id users
      }
      |> fetchNextUserBatch requestLimit
      |> fetchNextStreamBatch requestLimit
      |> persist
    Users (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    Streams (Ok streams) ->
      ( fetchNextGameBatch requestLimit
        <| fetchNextStreamBatch requestLimit
        { model
        | liveStreams = List.append model.liveStreams streams
        }
      , Cmd.none)
    Streams (Err error) ->
      { e = Debug.log "stream fetch error" error
      , r = (model, Cmd.none)}.r
    Games (Ok games) ->
      {model | games = List.append model.games <| List.map importGame games}
      |> persist
    Games (Err error) ->
      { e = Debug.log "game fetch error" error
      , r = (model, Cmd.none)}.r
    Videos userId (Ok videos) ->
      ( { model
        | events = Dict.insert userId (List.map (\v -> {start = v.createdAt, duration = v.duration}) videos) model.events
        }
      , Cmd.none
      )
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
    UI (View.HostClicked userId controlId) ->
      ( { model
        | pendingRequests =
          List.append model.pendingRequests [fetchVideos userId]
        }
      , Harbor.select controlId
      )
    UI (View.Refresh) ->
      (fetchNextStreamBatch requestLimit
        { model
        | liveStreams = []
        , pendingStreams = List.map .id model.users
        , previewVersion = model.previewVersion + 1
        }
      , Cmd.none)

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist model.users model.games
    |> Persist.Encode.persist
    |> Json.Encode.encode 0
    |> Harbor.save

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every (Time.second/requestRate) NextRequest
    , Harbor.loaded receiveLoaded
    ]

importUser : Twitch.Deserialize.User -> User
importUser user =
  { id = user.id
  , displayName = user.displayName
  }

importGame : Twitch.Deserialize.Game -> Game
importGame game =
  { id = game.id
  , name = game.name
  , boxArtUrl = game.boxArtUrl
  }

receiveLoaded : Maybe String -> Msg
receiveLoaded mstring =
  mstring
    |> Maybe.andThen (\string ->
      string
       |> Json.Decode.decodeString Persist.Decode.persist
       |> Result.mapError (Debug.log "persist decode error")
       |> Result.toMaybe
      )
    |> Loaded

resolveLoadedState : Model -> Model
resolveLoadedState model =
  let
    currentUsers = List.filter (\u -> Set.member (u.displayName |> String.toLower) desiredUserNames) model.users
    missing = missingUsers model
  in
    { model
    | users = currentUsers
    , missingUsers = missing
    , pendingUsers = missing
    , pendingStreams = List.map .id currentUsers
    }

missingUsers : Model -> List String
missingUsers model =
  let
    known = Set.fromList <| List.map (.displayName >> String.toLower) model.users
  in
    Set.toList <| Set.diff desiredUserNames known

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | missingUsers = missingUsers model
  , pendingUsers = List.drop batch model.pendingUsers
  } |> appendRequests [fetchUsers <| List.take batch model.pendingUsers]

fetchNextStreamBatch : Int -> Model -> Model
fetchNextStreamBatch batch model =
  { model
  | pendingStreams = List.drop batch model.pendingStreams
  } |> appendRequests [fetchStreams <| List.take batch model.pendingStreams]

fetchNextGameBatch : Int -> Model -> Model
fetchNextGameBatch batch model =
  let known = Set.fromList <| List.map .id model.games
      required = Set.fromList <| List.map .gameId model.liveStreams
      missing = Set.toList <| Set.diff required known
  in
    model
    |> appendRequests [fetchGames <| List.take batch missing]

appendRequests : List (Cmd Msg) -> Model -> Model
appendRequests cmds model = 
  { model
  | pendingRequests = List.append model.pendingRequests
    <| List.filter (\c -> c /= Cmd.none) cmds
  }

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
  if List.isEmpty users then
    Cmd.none
  else
    helix <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Twitch.Deserialize.users
      , tagger = Response << Users
      , url = (fetchUsersUrl users)
      }

fetchStreamsUrl : List String -> String
fetchStreamsUrl userIds =
  "https://api.twitch.tv/helix/streams?type=live&first=100&user_id=" ++ (String.join "&user_id=" userIds)

fetchStreams : List String -> Cmd Msg
fetchStreams userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    helix <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Twitch.Deserialize.liveStreams
      , tagger = Response << Streams
      , url = (fetchStreamsUrl userIds)
      }

fetchGamesUrl : List String -> String
fetchGamesUrl gameIds =
  "https://api.twitch.tv/helix/games?id=" ++ (String.join "&id=" gameIds)

fetchGames : List String -> Cmd Msg
fetchGames gameIds =
  if List.isEmpty gameIds then
    Cmd.none
  else
    helix <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Twitch.Deserialize.games
      , tagger = Response << Games
      , url = (fetchGamesUrl gameIds)
      }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=100&user_id=" ++ userId

fetchVideos : String -> Cmd Msg
fetchVideos userId =
  helix <|
    { clientId = TwitchId.clientId
    , auth = Nothing
    , decoder = Twitch.Deserialize.videos
    , tagger = Response << (Videos userId)
    , url = (fetchVideosUrl userId)
    }

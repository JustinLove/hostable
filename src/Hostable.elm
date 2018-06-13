module Hostable exposing (..)

import Persist exposing (Persist, User, Game)
import Persist.Encode
import Persist.Decode
import Twitch.Helix.Decode as Helix exposing (Stream)
import Twitch.Helix as Helix
import TwitchId
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
  | Imported (Result String (List User))
  | Users (Result Http.Error (List Helix.User))
  | Streams (Result Http.Error (List Stream))
  | Games (Result Http.Error (List Helix.Game))
  | Videos String (Result Http.Error (List Helix.Video))
  | Response Msg
  | NextRequest Time
  | UI (View.Msg)

type alias Model =
  { users : List User
  , games : List Game
  , liveStreams : List Stream
  , events : Dict String (List Event)
  , pendingUsers : List String
  , pendingStreams : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  , previewVersion : Int
  , selectedUser : Maybe String
  , selectedComment : Maybe (String, String)
  , addingComment : Maybe String
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
    , pendingUsers = []
    , pendingStreams = []
    , pendingRequests = []
    , outstandingRequests = 0
    , previewVersion = 0
    , selectedUser = Nothing
    , selectedComment = Nothing
    , addingComment = Nothing
    , time = 0
    }
  , Cmd.none
  )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( ( case mstate of
          Just state ->
            { model
            | users = state.users
            , games = state.games
            , events = state.events
            , pendingStreams = List.map .id state.users
            }
          Nothing ->
            model
        )
        |> fetchNextUserBatch requestLimit
        |> fetchNextStreamBatch requestLimit
      , Cmd.none)
    Imported (Ok users) ->
      { model
      | users = users
      , liveStreams = []
      , pendingStreams = List.map .id users
      }
      |> fetchNextStreamBatch requestLimit
      |> persist
    Imported (Err err) ->
      (model, Cmd.none)
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
    UI (View.HostClicked userId controlId) ->
      ( { model
        | pendingRequests =
          List.append model.pendingRequests [fetchVideos userId]
        , selectedUser = Just userId
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
    UI (View.AddChannel name) ->
      let lower = String.toLower name in
      if (List.filter (\u -> (String.toLower u.displayName) == lower) model.users) == [] then
        ( { model
          | pendingUsers = List.append model.pendingUsers [name]
          } |> fetchNextUserBatch requestLimit
        , Cmd.none
        )
      else
        (model, Cmd.none)
    UI (View.RemoveChannel userId) ->
      { model
      | users = List.filter (\u -> u.id /= userId) model.users
      , liveStreams = List.filter (\s -> s.userId /= userId) model.liveStreams
      , selectedUser = Nothing
      } |> persist
    UI (View.SelectComment userId comment) ->
      ( {model | selectedComment = Just (userId, comment)}, Cmd.none)
    UI (View.RemoveComment userId comment) ->
      { model
        | selectedComment = Nothing
        , users = removeComment userId comment model.users
      } |> persist
    UI (View.AddComment userId) ->
      ( { model | addingComment = Just userId }, Cmd.none)
    UI (View.CreateComment userId comment) ->
      { model
        | addingComment = Nothing
        , users = addComment userId comment model.users
      } |> persist
    UI (View.Import files) ->
      (model, Harbor.read files)

removeComment : String -> String -> List User -> List User
removeComment userId comment users =
  users |> List.map (\u -> if u.id == userId then
      { u | tags = List.filter (\t -> t /= comment) u.tags }
    else
      u
    )

addComment : String -> String -> List User -> List User
addComment userId comment users =
  users |> List.map (\u -> if u.id == userId then
      { u | tags = comment :: u.tags }
    else
      u
    )

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveState model)

saveState : Model -> Cmd Msg
saveState model =
  Persist model.users model.games model.events
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
    , Harbor.fileContents receiveImported
    ]

importUser : Helix.User -> User
importUser user =
  { id = user.id
  , displayName = user.displayName
  , tags = []
  }

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
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.users
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
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = Nothing
      , decoder = Helix.streams
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

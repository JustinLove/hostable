import Deserialize exposing (User, LiveStream, Game)
import TwitchId
import UserList
import View
import Harbor

import Html
import Http
import Time
import Set
import Json.Decode

requestLimit = 100
requestRate = 5

type Msg
  = Users (Result Http.Error (List User))
  | Streams (Result Http.Error (List LiveStream))
  | Games (Result Http.Error (List Game))
  | Response Msg
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { users : List User
  , games : List Game
  , liveStreams : List LiveStream
  , pendingUsers : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : (Model, Cmd Msg)
init =
  ( fetchNextUserBatch requestLimit
    { users = []
    , games = []
    , liveStreams = []
    , pendingUsers = List.map Tuple.first UserList.users
    , pendingRequests = []
    , outstandingRequests = 0
    }
  , Cmd.none
  )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Users (Ok users) ->
      (fetchNextUserBatch requestLimit
        { model
        | users = List.append model.users users
        , pendingRequests = List.append model.pendingRequests
          [fetchStreams <| List.map .id users]
        }
      , Cmd.none
      )
    Users (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    Streams (Ok streams) ->
      ( { model
        | liveStreams = List.append model.liveStreams streams
        }
      , Cmd.none)
    Streams (Err error) ->
      { e = Debug.log "stream fetch error" error
      , r = (model, Cmd.none)}.r
    Games (Ok games) ->
      ({model | games = List.append model.games games}
      , Cmd.none)
    Games (Err error) ->
      { e = Debug.log "game fetch error" error
      , r = (model, Cmd.none)}.r
    Response subMsg ->
      let
        (m2, c2) = update subMsg { model | outstandingRequests = model.outstandingRequests - 1}
      in
        (fetchNextGameBatch requestLimit m2, c2)
    NextRequest _ ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            }, next)
        _ -> (model, Cmd.none)
    UI (View.HostClicked controlId) ->
      (model, Harbor.select controlId)
    UI (View.Refresh) ->
      init

subscriptions : Model -> Sub Msg
subscriptions model =
  if List.isEmpty model.pendingRequests then
    Sub.none
  else
    Time.every (Time.second/requestRate) NextRequest

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | pendingUsers = List.drop batch model.pendingUsers
  , pendingRequests = List.append model.pendingRequests
    [fetchUsers <| List.take batch model.pendingUsers]
  }

fetchNextGameBatch : Int -> Model -> Model
fetchNextGameBatch batch model =
  if model.outstandingRequests == 0 && List.isEmpty model.pendingRequests then
    let known = Set.fromList <| List.map .id model.games
        required = Set.fromList <| List.map .gameId model.liveStreams
        missing = Set.toList <| Set.diff required known
    in
    { model
    | pendingRequests = List.append model.pendingRequests
      [fetchGames <| List.take batch missing]
    }
  else model

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
  if List.isEmpty users then
    Cmd.none
  else
    helix Users (fetchUsersUrl users) Deserialize.users

fetchStreamsUrl : List String -> String
fetchStreamsUrl userIds =
  "https://api.twitch.tv/helix/streams?type=live&first=100&user_id=" ++ (String.join "&user_id=" userIds)

fetchStreams : List String -> Cmd Msg
fetchStreams userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    helix Streams (fetchStreamsUrl userIds) Deserialize.liveStreams

fetchGamesUrl : List String -> String
fetchGamesUrl gameIds =
  "https://api.twitch.tv/helix/games?id=" ++ (String.join "&id=" gameIds)

fetchGames : List String -> Cmd Msg
fetchGames gameIds =
  if List.isEmpty gameIds then
    Cmd.none
  else
    helix Games (fetchGamesUrl gameIds) Deserialize.games

helix : ((Result Http.Error a) -> Msg) -> String -> Json.Decode.Decoder a -> Cmd Msg
helix tagger url decoder =
  Http.send (Response << tagger) <| Http.request
    { method = "GET"
    , headers =
      [ Http.header "Client-ID" TwitchId.clientId
      ]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

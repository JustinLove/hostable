import Deserialize exposing (User, LiveStream, Game)
import TwitchId
import UserList
import View
import Harbor

import Html
import Http
import Time
import Set

requestLimit = 100
requestRate = 5

type Msg
  = Users (Result Http.Error (List User))
  | Streams (Result Http.Error (List LiveStream))
  | Games (Result Http.Error (List Game))
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { users : List User
  , games : List Game
  , liveStreams : List LiveStream
  , pendingUsers : List String
  , pendingRequests : List (Cmd Msg)
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
        , pendingRequests = List.append model.pendingRequests
          [fetchGames <| Set.toList <| Set.fromList <| List.map .gameId streams]
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
    NextRequest _ ->
      case model.pendingRequests of
        next :: rest -> ({model | pendingRequests = rest}, next)
        _ -> (model, Cmd.none)
    UI (View.HostClicked controlId) ->
      (model, Harbor.select controlId)

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

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
  if List.isEmpty users then
    Cmd.none
  else
    Http.send Users <| Http.request
      { method = "GET"
      , headers =
        [ Http.header "Client-ID" TwitchId.clientId
        ]
      , url = fetchUsersUrl users
      , body = Http.emptyBody
      , expect = Http.expectJson Deserialize.users
      , timeout = Nothing
      , withCredentials = False
      }

fetchStreamsUrl : List String -> String
fetchStreamsUrl userIds =
  "https://api.twitch.tv/helix/streams?type=live&first=100&user_id=" ++ (String.join "&user_id=" userIds)

fetchStreams : List String -> Cmd Msg
fetchStreams userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    Http.send Streams <| Http.request
      { method = "GET"
      , headers =
        [ Http.header "Client-ID" TwitchId.clientId
        ]
      , url = fetchStreamsUrl userIds
      , body = Http.emptyBody
      , expect = Http.expectJson Deserialize.liveStreams
      , timeout = Nothing
      , withCredentials = False
      }

fetchGamesUrl : List String -> String
fetchGamesUrl gameIds =
  "https://api.twitch.tv/helix/games?id=" ++ (String.join "&id=" gameIds)

fetchGames : List String -> Cmd Msg
fetchGames gameIds =
  if List.isEmpty gameIds then
    Cmd.none
  else
    Http.send Games <| Http.request
      { method = "GET"
      , headers =
        [ Http.header "Client-ID" TwitchId.clientId
        ]
      , url = fetchGamesUrl gameIds
      , body = Http.emptyBody
      , expect = Http.expectJson Deserialize.games
      , timeout = Nothing
      , withCredentials = False
      }


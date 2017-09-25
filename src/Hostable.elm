import Deserialize exposing (User, LiveStream)
import TwitchId
import UserList
import View
import Harbor

import Html
import Http
import Time

type Msg
  = Users (Result Http.Error (List User))
  | Streams (Result Http.Error (List LiveStream))
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { users : List User
  , liveStreams : List LiveStream
  , pendingRequests : List (Cmd Msg)
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : (Model, Cmd Msg)
init = (Model [] [] [fetchUsers <| List.map Tuple.first UserList.users], Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Users (Ok users) ->
      ({model
        | users = List.append model.users users
        , pendingRequests = List.append model.pendingRequests
          [fetchStreams <| List.map .id users]
        }, Cmd.none)
    Users (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    Streams (Ok streams) ->
      ({model | liveStreams = List.append model.liveStreams streams}, Cmd.none)
    Streams (Err error) ->
      { e = Debug.log "stream fetch error" error
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
    Time.every (Time.second/2) NextRequest

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
  Http.send Users <| Http.request
    { method = "GET"
    , headers =
      --[ Http.header "Accept" "application/vnd.twitchtv.v5+json"
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
  "https://api.twitch.tv/kraken/streams?channel=" ++ (String.join "," userIds)

fetchStreams : List String -> Cmd Msg
fetchStreams userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    Http.send Streams <| Http.request
      { method = "GET"
      , headers =
        [ Http.header "Accept" "application/vnd.twitchtv.v5+json"
        , Http.header "Client-ID" TwitchId.clientId
        ]
      , url = fetchStreamsUrl userIds
      , body = Http.emptyBody
      , expect = Http.expectJson Deserialize.liveStreams
      , timeout = Nothing
      , withCredentials = False
      }

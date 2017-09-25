import Deserialize exposing (User, LiveStream)
import TwitchId
import UserList
import View exposing (Model)
import Harbor

import Html
import Http

type Msg
  = Users (Result Http.Error (List User))
  | Streams (Result Http.Error (List LiveStream))
  | UI (View.Msg)

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : (Model, Cmd Msg)
init = (Model [] [], fetchUsers <| List.map Tuple.first UserList.users)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Users (Ok users) ->
      ({model | users = users}, fetchStreams <| List.map .id users)
    Users (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    Streams (Ok streams) ->
      ({model | liveStreams = streams}, Cmd.none)
    Streams (Err error) ->
      { e = Debug.log "stream fetch error" error
      , r = (model, Cmd.none)}.r
    UI (View.HostClicked controlId) ->
      (model, Harbor.select controlId)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
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

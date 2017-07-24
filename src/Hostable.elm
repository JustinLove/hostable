import Deserialize exposing (LiveStream)
import TwitchId
import UserList
import View exposing (Model)

import Html
import Http

type Msg
  = Users (Result Http.Error (List String))
  | Streams (Result Http.Error (List LiveStream))

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.view
  }

init : (Model, Cmd Msg)
init = (Model [] [], fetchUsersIds <| List.map Tuple.first UserList.users)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Users (Ok ids) ->
      ({model | userIds = ids}, fetchStreams ids)
    Users (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    Streams (Ok streams) ->
      ({model | liveStreams = streams}, Cmd.none)
    Streams (Err error) ->
      { e = Debug.log "stream fetch error" error
      , r = (model, Cmd.none)}.r

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/kraken/users?login=" ++ (String.join "," users)

fetchUsersIds : List String -> Cmd Msg
fetchUsersIds users =
  Http.send Users <| Http.request
    { method = "GET"
    , headers =
      [ Http.header "Accept" "application/vnd.twitchtv.v5+json"
      , Http.header "Client-ID" TwitchId.clientId
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

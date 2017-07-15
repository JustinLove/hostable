import Deserialize
import TwitchId
import UserList

import Html
import Http

type alias Model =
  {
    userIds : List String
  }

type Msg =
  Users (Result Http.Error (List String))

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init : (Model, Cmd Msg)
init = (Model [], fetchUsersIds UserList.users)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Users (Ok ids) ->
      ({model | userIds = ids}, Cmd.none)
    Users (Err error) ->
      { e = Debug.log "fetch error" error
      , r = (model, Cmd.none)}.r

view : Model -> Html.Html msg
view model =
  Html.div [] [ Html.text (fetchUsersUrl UserList.users)]

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


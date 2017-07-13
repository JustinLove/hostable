import Html

type alias Model =
  {
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init : (Model, Cmd Msg)
init = ({}, Cmd.none)

type Msg = Nothing

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Html.Html msg
view model =
  Html.div [] []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

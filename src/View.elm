module View exposing (Model, view)

import Deserialize exposing (LiveStream)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model =
  { userIds : List String
  , liveStreams : List LiveStream
  }

view : Model -> Html msg
view model =
  div [] (List.map streamView model.liveStreams)

streamView : LiveStream -> Html msg
streamView stream =
  a [ href stream.url ]
    [ img [ src stream.preview, width 320, height 180] []
    , img [ src <| gameImageUrl stream.game, width 38, height 52, title stream.game ] []
    , p [] [ text stream.status]
    , p [] [ text stream.displayName]
    , p [] [ text stream.game]
    ]

gameImageUrl : String -> String
gameImageUrl game =
  "https://static-cdn.jtvnw.net/ttv-boxart/" ++ game ++ "-138x190.jpg"

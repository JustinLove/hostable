module View exposing (Model, view)

import Deserialize exposing (LiveStream)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model =
  { userIds : List String
  , liveStreams : List LiveStream
  }

css = """
#streams { display: flex; flex-wrap: wrap; list-style-type: none;}
.stream { width: 240px; height: 200px; padding: 10px; }
.info { display: flex; overflow-x: hidden; }
.game-image { flex-shrink: 0; margin-right: 0.5em; }
.info-text p { margin: 0.2em; font-size: 0.8em; white-space: nowrap; }
"""

view : Model -> Html msg
view model =
  div []
    [ node "style" [] [ text css ]
    , ul [ id "streams" ] <| List.map streamView model.liveStreams
    ]

streamView : LiveStream -> Html msg
streamView stream =
  li [ class "stream" ]
   [ a [ href stream.url ]
      [ img [ class "preview", src stream.preview, width 239, height 134 ] []
      , div [ class "info" ]
        [ img [ class "game-image", src <| gameImageUrl stream.game, width 38, height 52, title stream.game ] []
        , div [ class "info-text" ]
          [ p [ class "title" ] [ text stream.status]
          , p [ class "channel" ] [ text stream.displayName]
          , p [ class "game-text" ] [ text stream.game]
          ]
        ]
      ]
   ]

gameImageUrl : String -> String
gameImageUrl game =
  "https://static-cdn.jtvnw.net/ttv-boxart/" ++ game ++ "-138x190.jpg"

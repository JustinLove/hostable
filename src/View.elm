module View exposing (Model, Msg(..), view)

import Deserialize exposing (LiveStream)
import UserList

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

type alias Model =
  { userIds : List String
  , liveStreams : List LiveStream
  }

type Msg
  = HostClicked String

css = """
#streams { display: flex; flex-wrap: wrap; list-style-type: none;}
.stream { width: 240px; height: 200px; padding: 10px; }
.info { display: flex; overflow-x: hidden; }
.game-image { flex-shrink: 0; margin-right: 0.5em; }
.info-text p { margin: 0.2em; font-size: 0.8em; white-space: nowrap; }
.comments { list-style-type: none; padding-left: 0;}
.comments li {
  display: inline-block;
  margin: 0.2em;
  padding-left: 0.2em;
  padding-right: 0.2em;
  color: white;
  background-color: grey;
  border-radius: 0.2em;
}
"""

view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , ul [ id "streams" ] <| List.map streamView model.liveStreams
    ]

streamView : LiveStream -> Html Msg
streamView stream =
  li [ class "stream" ]
    [ a [ href stream.url ] [ img [ class "preview", src stream.preview, width 239, height 134 ] [] ]
    , div [ class "info" ]
      [ img [ class "game-image", src <| gameImageUrl stream.game, width 38, height 52, title stream.game ] []
      , div [ class "info-text" ]
        [ p [ class "title", title stream.status ] [ text stream.status]
        , input
          [ class "channel"
          , id ("host-" ++ stream.displayName)
          , Html.Events.onClick (HostClicked ("host-" ++ stream.displayName))
          , readonly True
          , value ("/host " ++ stream.displayName)
          ] []
        , ul [ class "comments" ]
          (List.map (li [] << List.singleton << text)
            <| commentsForStream stream.displayName UserList.users)
        ]
      ]
    ]

gameImageUrl : String -> String
gameImageUrl game =
  "https://static-cdn.jtvnw.net/ttv-boxart/" ++ game ++ "-138x190.jpg"

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

module View exposing (Msg(..), view)

import Deserialize exposing (User, LiveStream, Game)
import UserList

import Regex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

type Msg
  = HostClicked String

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
#streams { display: flex; flex-wrap: wrap; list-style-type: none;}
.stream { width: 240px; height: 200px; padding: 10px; }
.screen { width: 240px; height: 134px; position: relative; }
.box-art { position: absolute; bottom: 0; right: 0; }
.viewers {
  position: absolute;
  bottom: 0;
  left: 0;
  color: rgb(218, 216, 222);
  margin: 0.1em;
}
.info { display: flex; overflow-x: hidden; }
.info-text p { margin: 0.2em; font-size: 0.8em; white-space: nowrap; }
.channel {
  background-color: rgb(14, 12, 19);
  color: rgb(250, 249, 250);
  border: 1px solid #392e5c;
}
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

--view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , ul [ id "streams" ] <| List.map (streamView model) model.liveStreams
    ]

--streamView : Model -> LiveStream -> Html Msg
streamView model stream =
  let
    name = displayNameFor model.users stream
    game = gameFor model.games stream
  in
  li [ class "stream" ]
    [ a [ href ("https://twitch.tv/"++name) ]
      [ div [ class "screen" ]
        [ img [ class "preview", src (imageTemplateUrl 320 180 stream.thumbnailUrl), width 239, height 134 ] []
        , displayBoxArt game
        , p [ class "viewers" ] [ text <| toString stream.viewerCount ]
        ]
      ]
    , div [ class "info" ]
      [ div [ class "info-text" ]
        [ p [ class "title", title stream.title ] [ text stream.title]
        , input
          [ class "channel"
          , id ("host-" ++ name)
          , Html.Events.onClick (HostClicked ("host-" ++ name))
          , readonly True
          , value ("/host " ++ name)
          ] []
        , ul [ class "comments" ]
          (List.map (li [] << List.singleton << text)
            <| commentsForStream name UserList.users)
        ]
      ]
    ]

displayNameFor : List User -> LiveStream -> String
displayNameFor users stream =
  List.filterMap (\u -> if u.id == stream.userId then Just u.displayName else Nothing) users
   |> List.head
   |> Maybe.withDefault "unknown"

gameFor : List Game -> LiveStream -> Maybe Game
gameFor games stream =
  List.filterMap (\g -> if g.id == stream.gameId then Just g else Nothing) games
   |> List.head

displayBoxArt : Maybe Game -> Html Msg
displayBoxArt mgame =
  case mgame of
    Just game ->
      img
        [ class "box-art"
        , src <| imageTemplateUrl 38 52 game.boxArtUrl
        , width 38
        , height 52
        , title game.name
        ] []
    Nothing ->
      div
        [ class "box-art"
        , style [ ("width", "38px"), ("height", "52px") ]
        ] []

imageTemplateUrl : Int -> Int -> String -> String
imageTemplateUrl w h =
  Regex.replace Regex.All (Regex.regex "\\{width\\}") (\_ -> toString w)
  >> Regex.replace Regex.All (Regex.regex "\\{height\\}") (\_ -> toString h)

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

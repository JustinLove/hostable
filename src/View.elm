module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (LiveStream)
import Twitch.Template exposing (imageTemplateUrl)
import Persist exposing (User, Game)
import UserList

import Regex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed

type Msg
  = HostClicked String
  | Refresh

boxWidth = 70
boxHeight = 95

screenWidth = 168
screenHeight = 95

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
#streams { display: flex; flex-wrap: wrap; list-style-type: none;}
.stream { width: 240px; height: 200px; padding: 10px; }
.graphics { width: 240px; height: 95px; position: relative; }
.screen { width: 168px; height: 95px; position: absolute; right: 0;}
.box-art { position: absolute; left: 0; }
.viewers {
  margin-right: 0.2em;
  background: rgb(100, 65, 164);
  color: white;
  padding: 0.1em;
  padding-left: 0.3em;
  padding-right: 0.3em;
}
.info { display: flex; overflow-x: hidden; }
.info-text p { margin: 0.2em; font-size: 0.8em; white-space: nowrap; }
.game-name { color: rgb(174, 158, 202); }
.channel {
  background-color: rgb(14, 12, 19);
  color: rgb(250, 249, 250);
  border: 1px solid #392e5c;
}

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
    , header []
      [ button [onClick Refresh] [ text "Refresh" ]
      , text " Requests: "
      , text <| toString
        <| ((List.length model.pendingRequests) + model.outstandingRequests)
      ]
    , if List.isEmpty model.missingUsers then
        text ""
      else 
        displayMissingUsers model.missingUsers
    , model.liveStreams
      |> List.sortBy (\stream -> -stream.viewerCount)
      |> List.map (\stream -> (stream.channelId, (streamView model stream)))
      |> Keyed.ul [ id "streams" ]
    ]

displayMissingUsers : List String -> Html Msg
displayMissingUsers missingUsers =
  div []
    [ h2 [] [ text "Missing Users" ]
    , missingUsers
      |> List.intersperse " "
      |> List.map text
      |> p []
    ]

--streamView : Model -> LiveStream -> Html Msg
streamView model stream =
  let
    name = displayNameFor model.users stream
    game = gameFor model.games stream
  in
  li [ class "stream" ]
    [ div [ class "graphics" ]
      [ displayBoxArt game
      , a [ href ("https://twitch.tv/"++name) ]
        [ div [ class "screen" ]
          [ img [ class "preview", src ((imageTemplateUrl screenWidth screenHeight stream.thumbnailUrl) ++ "?" ++ (toString model.previewVersion)), width screenWidth, height screenHeight ] []
          ]
        ]
      ]
    , div [ class "info" ]
      [ div [ class "info-text" ]
        [ div [ class "viewers-channel" ]
          [ span [ class "viewers" ] [ text <| toString stream.viewerCount ]
          , input
            [ class "channel"
            , id ("host-" ++ name)
            , Html.Events.onClick (HostClicked ("host-" ++ name))
            , readonly True
            , value ("/host " ++ name)
            ] []
          ]
        , let gameName = nameOfGame game in p [ class "game-name", title gameName] [ text gameName ]
        , p [ class "title", title stream.title ] [ text stream.title]
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
        , src <| imageTemplateUrl boxWidth boxHeight game.boxArtUrl
        , width boxWidth
        , height boxHeight
        , title game.name
        ] []
    Nothing ->
      div
        [ class "box-art"
        , style [ ("width", (toString boxWidth) ++ "px"), ("height", (toString boxHeight) ++ "px") ]
        ] []

nameOfGame : Maybe Game -> String
nameOfGame mgame =
  case mgame of
    Just game ->
      game.name
    Nothing ->
      "--"

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

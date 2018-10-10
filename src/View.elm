module View exposing (Msg(..), view, document)

import FileInput
import Twitch.Helix.Decode exposing (Stream)
import Twitch.Template exposing (imageTemplateUrl)
import Persist exposing (Export, User, Game)
import Persist.Encode
import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, preventDefaultOn)
import Html.Keyed as Keyed
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Time
import Color
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Base64

type Msg
  = Refresh
  | Import Json.Decode.Value
  | AddChannel String
  | RemoveChannel String
  | HostClicked String String
  | SelectComment String String
  | RemoveComment String String
  | AddComment String
  | CreateComment String String
  | SelectGame String
  | UpdateGameScore String Float

boxWidth = 70
boxHeight = 95

screenWidth = 168
screenHeight = 95

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
header { display: flex; justify-content: space-between;}
.streams { display: flex; flex-wrap: wrap; list-style-type: none;}
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
.schedule-graph { width: 240px; }

.comments { padding: 0;}
.comments li {
  display: inline-block;
  margin: 0.2em;
  padding-left: 0.2em;
  padding-right: 0.2em;
  color: white;
  background-color: grey;
  border-radius: 0.2em;
}

.games { display: flex; flex-wrap: wrap; list-style-type: none;}
.game { padding: 10px; position: relative;}
.score {
  background: rgb(100, 65, 164);
  color: white;
  padding: 0.1em;
  padding-left: 0.3em;
  padding-right: 0.3em;
  position: absolute;
  bottom: 10px;
  left: 0;
}
#edit-game-score {
  padding: 0.1em;
  padding-left: 0.3em;
  padding-right: 0.3em;
  position: absolute;
  bottom: 10px;
  left: 0;
  width: 60px;
}

svg.icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}
.icon-github { color: #888; }
.icon-twitter { color: #55acee; }
.icon-twitch { color: #6441A4; }
a:link, a:visited { color: #b19dd8; }
a:hover, a:active { color: rgb(218, 216, 222); }
"""

document tagger model =
  { title = "Hostable"
  , body = [Html.map tagger (view model)]
  }

--view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , header []
      [ div [ class "refresh" ] 
        [ button [onClick Refresh] [ text "Refresh" ]
        , text " Requests: "
        , text <| String.fromInt
          <| ((List.length model.pendingRequests) + model.outstandingRequests)
        ]
      , div [ class "add-channel" ]
        [ label [ for "channelname" ] [ text "Add Channel" ]
        , input
          [ type_ "text"
          , id "channelname"
          , name "channelname"
          , value ""
          , on "change" <| targetValue Json.Decode.string AddChannel
          ] []
        ]
      , input
        [ type_ "file"
        , on "change" (FileInput.targetFiles Import)
        ]
        []
      , a
          [ href ("data:;base64," ++ (model |> export |> Base64.encode))
          , download "hostable.json"
          ]
          [ text "export" ]
      ]
    , model.liveStreams
      |> Dict.values
      |> List.filter (\stream -> Dict.get stream.userId model.users |> Maybe.map .persisted |> Maybe.withDefault False)
      |> List.sortBy ((rankStream model)>>negate)
      |> List.map (\stream -> (stream.channelId, (streamView model stream)))
      |> Keyed.ul [ id "streams", class "streams" ]
    , model.games
      |> Dict.toList
      |> List.sortBy (\(_, game) -> game.name)
      |> List.map (\(key, game) -> (key, gameView model game))
      |> Keyed.ul [ id "games", class "games" ]
    , displayFooter
    ]

--streamView : Model -> Stream -> Html Msg
streamView model stream =
  let
    muser = userFor model.users stream
    name = muser |> Maybe.map .displayName |> Maybe.withDefault "unknown"
    tags = muser |> Maybe.map .tags |> Maybe.withDefault []
    game = gameFor model.games stream
    mevents = Dict.get stream.userId model.events
  in
  li [ class "stream" ]
    [ div [ class "graphics" ]
      [ displayBoxArt game
      , a [ href ("https://twitch.tv/"++name) ]
        [ div [ class "screen" ]
          [ img
            [ class "preview"
            , src
              ( (imageTemplateUrl screenWidth screenHeight stream.thumbnailUrl)
              ++ "?" ++ (String.fromInt model.previewVersion)
              )
            , width screenWidth
            , height screenHeight
            ] []
          ]
        ]
      ]
    , div [ class "info" ]
      [ div [ class "info-text" ]
        [ div [ class "viewers-channel" ]
          [ span [ class "viewers" ] [ text <| String.fromInt stream.viewerCount ]
          , input
            [ class "channel"
            , id ("host-" ++ name)
            , Html.Events.onClick (HostClicked stream.userId ("host-" ++ name))
            , readonly True
            , value ("/host " ++ name)
            ] []
          , if model.selectedUser == Just stream.userId then
              button [ onClick (RemoveChannel stream.userId) ] [ text "X" ]
            else
              text ""
          ]
        , let gameName = nameOfGame game in p [ class "game-name", title gameName] [ text gameName ]
        , p [ class "title", title stream.title ] [ text stream.title]
        , case mevents of
            Just [] -> p [] [text "no videos"]
            Just events ->
              scheduleGraph [ Svg.Attributes.class "schedule-graph"] <|
                { width = 240
                , height = 40
                , labelWidths = model.labelWidths
                , time = model.time
                , zone = model.zone
                , days = [Time.toWeekday model.zone model.time]
                , events = List.filter (\e -> e.duration < (56 * 60 * 60 * 1000)) events
                , style =
                  { dataColor = Color.rgb 100 65 164
                  , labelColor = Color.rgb 218 216 222
                  , ruleColor = Color.grayscale 0.7
                  , currentDayColor = Color.grayscale 0.9
                  , currentTimeColor = Color.red
                  }
                }
            Nothing -> text ""
        , ul [ class "comments" ]
          (tags
            |> List.map (displayComment model.selectedComment stream.userId)
            |> (::) (displayAddingComment model.addingComment stream.userId)
          )
        ]
      ]
    ]

displayComment : Maybe (String, String) -> String -> String -> Html Msg
displayComment selectedComment userId comment =
  let
    selected = case selectedComment of
      Just (id, com) -> id == userId && com == comment
      Nothing -> False
  in
    if selected then
      li []
        [ text comment
        , button [ onClick (RemoveComment userId comment) ] [ text "X" ]
        ]
    else
      li [ onClick (SelectComment userId comment) ] <| List.singleton <| text comment

displayAddingComment : Maybe String -> String -> Html Msg
displayAddingComment addingComment userId =
  if addingComment == Just userId then
    li [ ]
      [ input
        [ type_ "text"
        , id "new-comment"
        , name "new-comment"
        , on "change" <| targetValue Json.Decode.string (CreateComment userId)
        ] []
      ]
  else
    li [ onClick (AddComment userId) ] [ text "+"]

--rankStream : Model -> Stream -> Float
rankStream model stream =
  let
    muser = userFor model.users stream
    tags = muser |> Maybe.map .tags |> Maybe.withDefault []
    game = gameFor model.games stream |> Maybe.andThen .score |> Maybe.withDefault 1.0
  in
  List.foldr
    (*)
    (game / (toFloat (stream.viewerCount + 1)))
    (List.filterMap (\tag -> Dict.get tag model.scoredTags) tags)

--gameView : Model -> Game -> Html Msg
gameView model game =
  li
    [ class "game"
    , style "width" ((String.fromInt boxWidth) ++ "px")
    , style "height" ((String.fromInt boxHeight) ++ "px")
    ]
    <|
    if model.selectedGame == Just game.id then
      [ displayBoxArt (Just game)
      , input
        [ type_ "number"
        , id "edit-game-score"
        , name "edit-game-score"
        , step "0.1"
        , value (game.score |> Maybe.withDefault 1.0 |> String.fromFloat)
        , on "blur" <| targetValue decodeFloat (UpdateGameScore game.id)
        ] []
      ]
    else
      [ a
        [ preventDefaultOn "click"
          (Json.Decode.succeed (SelectGame game.id, True))
        , href "#"
        ]
        [ displayBoxArt (Just game)
        , case game.score of
          Just score ->
            div [ class "score" ] [ text <| String.fromFloat score ]
          Nothing ->
            text ""
        ]
      ]

userFor : Dict String User -> Stream -> Maybe User
userFor users stream =
  Dict.get stream.userId users

gameFor : Dict String Game -> Stream -> Maybe Game
gameFor games stream =
  Dict.get stream.gameId games

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
        ]
        []
    Nothing ->
      div
        [ class "box-art"
        , style "width" ((String.fromInt boxWidth) ++ "px")
        , style "height" ((String.fromInt boxHeight) ++ "px")
        ]
        []

nameOfGame : Maybe Game -> String
nameOfGame mgame =
  case mgame of
    Just game ->
      game.name
    Nothing ->
      "--"

displayFooter : Html msg
displayFooter =
  footer []
  [ a [ href "https://github.com/JustinLove/hostable" ]
    [ icon "github", text "hostable" ]
  , text " "
  , a [ href "https://twitter.com/wondible" ]
    [ icon "twitter", text "@wondible" ]
  , text " "
  , a [ href "https://twitch.tv/wondible" ]
    [ icon "twitch", text "wondible" ]
  ]

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]

--export : Model -> String
export model =
  Export
      (model.users |> Dict.values)
      (model.games |> Dict.values |> List.filter (\g -> g.score /= Nothing))
    |> Persist.Encode.export
    |> Json.Encode.encode 2

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

decodeFloat : Json.Decode.Decoder Float
decodeFloat =
  Json.Decode.string
    |> Json.Decode.andThen (\s -> case String.toFloat s of
      Just n -> Json.Decode.succeed n
      Nothing -> Json.Decode.fail s
    )

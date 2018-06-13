module View exposing (Msg(..), view)

import Twitch.Helix.Decode exposing (Stream)
import Twitch.Template exposing (imageTemplateUrl)
import Persist exposing (User, Game)
import UserList
import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Html.Keyed as Keyed
import Svg.Attributes
import Color
import Date exposing (Day(..))
import Dict
import Json.Decode

type Msg
  = HostClicked String String
  | Refresh
  | AddChannel String

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
.schedule-graph { width: 240px; }

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
      [ div [ class "refresh" ] 
        [ button [onClick Refresh] [ text "Refresh" ]
        , text " Requests: "
        , text <| toString
        <| ((List.length model.pendingRequests) + model.outstandingRequests)
        ]
      , div [ class "add-channel" ]
        [ label [ for "channelname" ] [ text "Add Channel" ]
        , input
          [ type_ "text"
          , id "channelname"
          , name "channelname"
          , on "change" <| targetValue Json.Decode.string AddChannel
          ] []
        ]
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
            , Html.Events.onClick (HostClicked stream.userId ("host-" ++ name))
            , readonly True
            , value ("/host " ++ name)
            ] []
          ]
        , let gameName = nameOfGame game in p [ class "game-name", title gameName] [ text gameName ]
        , p [ class "title", title stream.title ] [ text stream.title]
        , case mevents of
            Just [] -> p [] [text "no videos"]
            Just events ->
              scheduleGraph [ Svg.Attributes.class "schedule-graph"] <|
                { width = 240
                , height = 40
                , time = model.time
                , days = [Date.dayOfWeek <| Date.fromTime model.time]
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
          (List.map (li [] << List.singleton << text)
            <| tags)
        ]
      ]
    ]

displayNameFor : List User -> Stream -> String
displayNameFor users stream =
  List.filterMap (\u -> if u.id == stream.userId then Just u.displayName else Nothing) users
   |> List.head
   |> Maybe.withDefault "unknown"

userFor : List User -> Stream -> Maybe User
userFor users stream =
  List.filter (\u -> u.id == stream.userId) users
   |> List.head

gameFor : List Game -> Stream -> Maybe Game
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

targetValue : Json.Decode.Decoder a -> (a -> Msg) -> Json.Decode.Decoder Msg
targetValue decoder tagger =
  Json.Decode.map tagger
    (Json.Decode.at ["target", "value" ] decoder)

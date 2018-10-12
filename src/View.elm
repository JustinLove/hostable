module View exposing (Msg(..), AppMode(..), view, document)

import FileInput
import Twitch.Helix.Decode exposing (Stream)
import Twitch.Template exposing (imageTemplateUrl)
import Persist exposing (Export, User, Game)
import Persist.Encode
import ScheduleGraph exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, on, preventDefaultOn)
import Html.Keyed as Keyed
import Set
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
  | Navigate AppMode

type AppMode
  = LiveStreams
  | GameScores
  | TagScores

boxWidth = 70
boxHeight = 95

screenWidth = 168
screenHeight = 95

document tagger model =
  { title = "Hostable"
  , body = [Html.map tagger (view model)]
  }

--view : Model -> Html Msg
view model =
  div []
    [ headerView model
    , case model.appMode of
      LiveStreams -> liveStreamsView model
      GameScores -> gamesView model
      TagScores -> tagsView model
    , displayFooter
    ]

headerView model =
  header []
    [ div [ class "refresh" ] 
      [ button [onClick Refresh] [ text "Refresh" ]
      , text " Requests: "
      , text <| String.fromInt
        <| ((List.length model.pendingRequests) + model.outstandingRequests)
      ]
    , nav []
      [ ul []
        [ navigationItem model.appMode LiveStreams "live-streams" "Streams"
        , navigationItem model.appMode GameScores "game-scores" "Games"
        , navigationItem model.appMode TagScores "tag-scores" "Tags"
        ]
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

navigationItem : AppMode -> AppMode -> String -> String -> Html Msg
navigationItem current target itemId title =
  li
    [ classList [ ("selected", current == target) ]
    --, ariaSelected (if current == target then "true" else "false")
    ]
    [ div [ class "navigation-controls" ]
      [ input
        [ type_ "radio"
        , Html.Attributes.name "navigation"
        , id (itemId ++ "-navigation")
        , value title
        , onCheck (\_ -> Navigate target)
        , checked (current == target)
        ] []
      , label [ for (itemId ++ "-navigation") ]
        [ text title
        ]
      ]
    ]

liveStreamsView model =
  model.liveStreams
    |> Dict.values
    |> List.filter (\stream -> Dict.get stream.userId model.users |> Maybe.map .persisted |> Maybe.withDefault False)
    |> List.sortBy ((rankStream model)>>negate)
    |> List.map (\stream -> (stream.channelId, (streamView model stream)))
    |> Keyed.ul [ id "streams", class "streams" ]

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

gamesView model =
  model.games
    |> Dict.toList
    |> List.sortBy (\(_, game) -> game.name)
    |> List.map (\(key, game) -> (key, gameView model game))
    |> Keyed.ul [ id "games", class "games" ]

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

tagsView model =
  model.users
    |> Dict.values
    |> List.concatMap .tags
    |> Set.fromList
    |> Set.toList
    |> List.map (\tag -> (tag, tagView model tag))
    |> Keyed.ul [ id "tags", class "tags" ]

tagView model tag =
  li []
    [ text tag
    , text " "
    , case Dict.get tag model.scoredTags of
        Just score ->
          span [ class "score" ] [ text <| String.fromFloat score ]
        Nothing ->
          text ""
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

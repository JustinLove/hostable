module Persist.Encode exposing (persist, export, user, game)

import Persist exposing (Persist, Export, User, Game, Event)

import Json.Encode exposing (..)
import Dict exposing (Dict)
import Time

persist : Persist -> Value
persist p =
  object
    <| List.concat
      [
        [ ("users", list user <| List.filter .persisted p.users)
        , ("games", list game p.games)
        , ("scoredTags", dict identity float p.scoredTags)
        , ("events", events p.events)
        ]
      , (case p.auth of
          Just auth -> [("auth", string auth)]
          Nothing -> []
        )
      , (case p.autoChannel of
          Just channel -> [("autoChannel", string channel)]
          Nothing -> []
        )
      ]

export : Export -> Value
export e =
  object
    [ ("users", list user <| List.filter .persisted e.users)
    , ("games", list game e.games)
    , ("scoredTags", dict identity float e.scoredTags)
    ]

user : User -> Value
user u =
  object
    [ ("id", string u.id)
    , ("displayName", string u.displayName)
    , ("tags", list string u.tags)
    ]

game : Game -> Value
game g =
  object <|
    List.append
      [ ("id", string g.id)
      , ("name", string g.name)
      , ("boxArtUrl", string g.boxArtUrl)
      ]
      (case g.score of
        Just score -> [ ("score", float score) ]
        Nothing -> []
      )

events : Dict String (List Event) -> Value
events evts =
  evts
    |> Dict.map (\_ es -> list event es)
    |> Dict.toList
    |> object

event : Event -> Value
event e =
  object
    [ ("start", int <| Time.posixToMillis e.start)
    , ("duration", int e.duration)
    ]

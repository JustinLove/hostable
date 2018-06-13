module Persist.Encode exposing (persist, export, user, game)

import Persist exposing (Persist, User, Game, Event)

import Json.Encode exposing (..)
import Dict exposing (Dict)

persist : Persist -> Value
persist p =
  object
    [ ("users", list <| List.map user p.users)
    , ("games", list <| List.map game p.games)
    , ("events", events p.events)
    ]

export : List User -> Value
export us =
  object
    [ ("users", list <| List.map user us)
    ]

user : User -> Value
user u =
  object
    [ ("id", string u.id)
    , ("displayName", string u.displayName)
    , ("tags", list <| List.map string u.tags)
    ]

game : Game -> Value
game g =
  object
    [ ("id", string g.id)
    , ("name", string g.name)
    , ("boxArtUrl", string g.boxArtUrl)
    ]

events : Dict String (List Event) -> Value
events evts =
  evts
    |> Dict.map (\_ es -> list <| List.map event es)
    |> Dict.toList
    |> object


event : Event -> Value
event e =
  object
    [ ("start", float e.start)
    , ("duration", float e.duration)
    ]

module Persist.Encode exposing (persist, export, user, game, community)

import Persist exposing (Persist, Export, User, Game, Community, Event)

import Json.Encode exposing (..)
import Dict exposing (Dict)

persist : Persist -> Value
persist p =
  object
    [ ("users", list <| List.map user <| List.filter .persisted p.users)
    , ("games", list <| List.map game p.games)
    , ("communities", list <| List.map community p.communities)
    , ("events", events p.events)
    ]

export : Export -> Value
export e =
  object
    [ ("users", list <| List.map user <| List.filter .persisted e.users)
    , ("communities", list <| List.map community e.communities)
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

community : Community -> Value
community c =
  object
    [ ("id", string c.id)
    , ("name", string c.name)
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

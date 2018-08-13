module Persist.Decode exposing (persist, export, user, game, community, event)

import Persist exposing (Persist, Export, User, Game, Community, Event)

import Json.Decode exposing (..)
import Dict exposing (Dict)

persist : Decoder Persist
persist =
  map4 Persist
    (field "users" (list user))
    (field "games" (list game))
    (oneOf
      [ (field "communities" (list community))
      , succeed []
      ])
    (oneOf
      [ (field "events" (dict (list event)))
      , succeed Dict.empty
      ])

export : Decoder Export
export =
  map2 Export
    (field "users" (list user))
    (oneOf
      [ (field "communities" (list community))
      , succeed []
      ])

user : Decoder User
user =
  map4 User
    (field "id" string)
    (field "displayName" string)
    (oneOf
      [ field "tags" (list string)
      , succeed []
      ]
    )
    (succeed True)

game : Decoder Game
game =
  map3 Game
    (field "id" string)
    (field "name" string)
    (field "boxArtUrl" string)

community : Decoder Community
community =
  map2 Community
    (field "id" string)
    (field "name" string)

event : Decoder Event
event =
  map2 Event
    (field "start" float)
    (field "duration" float)

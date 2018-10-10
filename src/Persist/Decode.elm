module Persist.Decode exposing (persist, export, user, game, event)

import Persist exposing (Persist, Export, User, Game, Event)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Time

persist : Decoder Persist
persist =
  map3 Persist
    (field "users" (list user))
    (field "games" (list game))
    (oneOf
      [ (field "events" (dict (list event)))
      , succeed Dict.empty
      ])

export : Decoder Export
export =
  map Export
    (field "users" (list user))

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
  map4 Game
    (field "id" string)
    (field "name" string)
    (field "boxArtUrl" string)
    (maybe (field "score" float))

event : Decoder Event
event =
  map2 Event
    (field "start" (map Time.millisToPosix int))
    (field "duration" int)

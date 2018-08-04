module Persist.Decode exposing (persist, export, user, game, event)

import Persist exposing (Persist, User, Game, Event)

import Json.Decode exposing (..)
import Dict exposing (Dict)

persist : Decoder Persist
persist =
  map3 Persist
    (field "users" (list user))
    (field "games" (list game))
    (oneOf
      [ (field "events" (dict (list event)))
      , succeed Dict.empty
      ])

export : Decoder (List User)
export =
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
  map3 Game
    (field "id" string)
    (field "name" string)
    (field "boxArtUrl" string)

event : Decoder Event
event =
  map2 Event
    (field "start" float)
    (field "duration" float)

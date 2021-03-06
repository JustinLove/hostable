module Persist.Decode exposing (persist, export, user, game, event, followCount)

import Persist exposing (Persist, Export, User, Game, Event, FollowCount)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Time

persist : Decoder Persist
persist =
  map7 Persist
    (field "users" (list user))
    (field "games" (list game))
    (oneOf
      [ (field "scoredTags" (dict float))
      , succeed Dict.empty
      ])
    (oneOf
      [ (field "events" (dict (list event)))
      , succeed Dict.empty
      ])
    (oneOf
      [ (field "followers" (dict followCount))
      , succeed Dict.empty
      ])
    (maybe (field "auth" string))
    (maybe (field "autoChannel" string))

export : Decoder Export
export =
  map3 Export
    (field "users" (list user))
    (oneOf
      [ (field "games" (list game))
      , succeed []
      ])
    (oneOf
      [ (field "scoredTags" (dict float))
      , succeed Dict.empty
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

followCount : Decoder FollowCount
followCount =
  map2 FollowCount
    (field "count" int)
    (field "lastUpdated" (map Time.millisToPosix int))

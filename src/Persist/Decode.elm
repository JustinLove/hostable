module Persist.Decode exposing (persist, user, game)

import Persist exposing (Persist)
import Deserialize exposing (User, Game)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map2 Persist
    (field "users" (list user))
    (field "games" (list game))

user : Decoder User
user =
  map2 User
    (field "id" string)
    (field "displayName" string)

game : Decoder Game
game =
  map3 Game
    (field "id" string)
    (field "name" string)
    (field "boxArtUrl" string)

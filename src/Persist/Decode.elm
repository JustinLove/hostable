module Persist.Decode exposing (user, game)

import Deserialize exposing (User, Game)

import Json.Decode exposing (..)

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

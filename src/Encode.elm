module Encode exposing (user)

import Deserialize exposing (User, Game)

import Json.Encode exposing (..)

user : User -> Value
user u =
  object
    [ ("id", string u.id)
    , ("displayName", string u.displayName)
    ]

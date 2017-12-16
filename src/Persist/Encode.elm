module Persist.Encode exposing (persist, user, game)

import Persist exposing (Persist)
import Deserialize exposing (User, Game)

import Json.Encode exposing (..)

persist : Persist -> Value
persist p =
  object
    [ ("users", list <| List.map user p.users)
    , ("games", list <| List.map game p.games)
    ]

user : User -> Value
user u =
  object
    [ ("id", string u.id)
    , ("displayName", string u.displayName)
    ]

game : Game -> Value
game g =
  object
    [ ("id", string g.id)
    , ("name", string g.name)
    , ("boxArtUrl", string g.boxArtUrl)
    ]

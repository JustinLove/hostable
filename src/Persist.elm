module Persist exposing (Persist)

import Deserialize exposing (User, Game)

type alias Persist =
  { users : List User
  , games : List Game
  }

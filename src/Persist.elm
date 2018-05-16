module Persist exposing (Persist, User, Game, Event)

import Dict exposing (Dict)
import Time exposing (Time)

type alias Persist =
  { users : List User
  , games : List Game
  , events : Dict String (List Event)
  }

type alias User =
  { id : String
  , displayName : String
  }

type alias Game =
  { id : String
  , name : String
  , boxArtUrl : String
  }

type alias Event =
  { start : Time
  , duration : Time
  }

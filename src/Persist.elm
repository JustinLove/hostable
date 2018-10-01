module Persist exposing (Persist, Export, User, Game, Event)

import Dict exposing (Dict)
import Time exposing (Posix)

type alias Persist =
  { users : List User
  , games : List Game
  , events : Dict String (List Event)
  }

type alias Export =
  { users : List User
  }

type alias User =
  { id : String
  , displayName : String
  , tags : List String
  , persisted : Bool
  }

type alias Game =
  { id : String
  , name : String
  , boxArtUrl : String
  }

type alias Event =
  { start : Posix
  , duration : Int
  }

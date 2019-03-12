module Persist exposing (Persist, Export, User, Game, Event, FollowCount)

import Dict exposing (Dict)
import Time exposing (Posix)

type alias Persist =
  { users : List User
  , games : List Game
  , scoredTags : Dict String Float
  , events : Dict String (List Event)
  , followers : Dict String FollowCount
  , auth : Maybe String
  , autoChannel : Maybe String
  }

type alias Export =
  { users : List User
  , games : List Game
  , scoredTags : Dict String Float
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
  , score : Maybe Float
  }

type alias Event =
  { start : Posix
  , duration : Int
  }

type alias FollowCount =
  { count : Int
  , lastUpdated : Posix
  }

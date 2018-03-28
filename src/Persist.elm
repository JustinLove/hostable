module Persist exposing (Persist, User, Game)

type alias Persist =
  { users : List User
  , games : List Game
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

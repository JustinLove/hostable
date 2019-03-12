module Persist.Encode exposing (persist, export, user, game)

import Persist exposing (Persist, Export, User, Game, Event, FollowCount)

import Json.Encode exposing (..)
import Dict exposing (Dict)
import Time

persist : Persist -> Value
persist p =
  object
    <| List.concat
      [
        [ ("users", list user <| List.filter .persisted p.users)
        , ("games", list game p.games)
        , ("scoredTags", dict identity float p.scoredTags)
        , ("events", events p.events)
        , ("followers", followers p.followers)
        ]
      , (case p.auth of
          Just auth -> [("auth", string auth)]
          Nothing -> []
        )
      , (case p.autoChannel of
          Just channel -> [("autoChannel", string channel)]
          Nothing -> []
        )
      ]

export : Export -> Value
export e =
  object
    [ ("users", list user <| List.filter .persisted e.users)
    , ("games", list game e.games)
    , ("scoredTags", dict identity float e.scoredTags)
    ]

user : User -> Value
user u =
  object
    [ ("id", string u.id)
    , ("displayName", string u.displayName)
    , ("tags", list string u.tags)
    ]

game : Game -> Value
game g =
  object <|
    List.append
      [ ("id", string g.id)
      , ("name", string g.name)
      , ("boxArtUrl", string g.boxArtUrl)
      ]
      (case g.score of
        Just score -> [ ("score", float score) ]
        Nothing -> []
      )

events : Dict String (List Event) -> Value
events evts =
  evts
    |> Dict.map (\_ es -> list event es)
    |> Dict.toList
    |> object

event : Event -> Value
event e =
  object
    [ ("start", int <| Time.posixToMillis e.start)
    , ("duration", int e.duration)
    ]

followers : Dict String FollowCount -> Value
followers fls =
  fls
    |> Dict.map (\_ f -> followCount f)
    |> Dict.toList
    |> object

followCount : FollowCount -> Value
followCount f =
  object
    [ ("count", int f.count)
    , ("lastUpdated", int <| Time.posixToMillis f.lastUpdated)
    ]

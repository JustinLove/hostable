module Decode exposing
  ( users
  , logins
  , games
  , events
  , streams
  , Stream
  )

import Persist
import ScheduleGraph

import Twitch.Helix as Helix
import Twitch.Helix.User as User
import Twitch.Helix.Game as Game
import Twitch.Helix.Video as Video
import Twitch.Helix.Stream as Stream

import Json.Decode exposing (..)

users : Decoder (List Persist.User)
users = User.response user

user : Decoder Persist.User
user =
  map4 Persist.User
    User.id
    User.displayName
    (succeed []) -- tags
    (succeed False) -- persisted

logins : Decoder (List String)
logins = User.response User.login

games : Decoder (List Persist.Game)
games = Game.response game

game : Decoder Persist.Game
game =
  map4 Persist.Game
    Game.id
    Game.name
    Game.boxArtUrl
    (succeed Nothing) -- score

events : Decoder (List ScheduleGraph.Event)
events =
  Video.response video
    |> map (List.filterMap (\(videoType, ev) -> if videoType == Video.Archive then Just ev else Nothing))

video : Decoder (Video.VideoType, ScheduleGraph.Event)
video =
  map2 Tuple.pair
    Video.videoType
    event

event : Decoder ScheduleGraph.Event
event =
  map2 ScheduleGraph.Event
    Video.createdAt
    Video.duration

type alias Stream =
  { id : Helix.StreamId
  , userId : Helix.UserId 
  , userName : String 
  , gameId : Helix.GameId
  , viewerCount : Int
  , thumbnailUrl : String
  , title : String
  }

streams : Decoder (List Stream)
streams = User.response stream

stream : Decoder Stream
stream =
  succeed Stream
    |> map2 (|>) Stream.id
    |> map2 (|>) Stream.userId
    |> map2 (|>) Stream.userName
    |> map2 (|>) Stream.gameId
    |> map2 (|>) Stream.viewerCount
    |> map2 (|>) Stream.thumbnailUrl
    |> map2 (|>) Stream.title

module Twitch.Deserialize exposing (User, LiveStream, Game, users, liveStreams, games)

import Json.Decode exposing (..)

{-"data":[{
   "id":"44322889",
   "login":"dallas",
   "display_name":"dallas",
   "type":"staff",
   "broadcaster_type":"",
   "description":"Just a gamer playing games and chatting. :)",
   "profile_image_url":"https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-profile_image-1a2c906ee2c35f12-300x300.png",
   "offline_image_url":"https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-channel_offline_image-1a2c906ee2c35f12-1920x1080.png",
   "view_count":191836881,
   "email":"login@provider.com"
}]-}


type alias User =
  { id : String
  , displayName : String
  }

users : Decoder (List User)
users =
  field "data" (list user)

user : Decoder User
user =
  map2 User
    (field "id" string)
    (field "display_name" string)

{-"data":
   [
      {
         "id":"26007494656",
         "user_id":"23161357",
         "game_id":"417752",
         "community_ids":[
            "5181e78f-2280-42a6-873d-758e25a7c313",
            "848d95be-90b3-44a5-b143-6e373754c382",
            "fd0eab99-832a-4d7e-8cc0-04d73deb2e54"
         ],
         "type":"live",
         "title":"Hey Guys, It's Monday - Twitter: @Lirik",
         "viewer_count":32575,
         "started_at":"2017-08-14T16:08:32Z",
         "language":"en",
         "thumbnail_url":"https://static-cdn.jtvnw.net/previews-ttv/live_user_lirik-{width}x{height}.jpg"
      }, 
      ... 
   ], 
   "pagination":{"cursor":"eyJiIjpudWxsLCJhIjp7Ik9mZnNldCI6MjB9fQ=="}
-}

type alias LiveStream =
  { channelId : String
  , userId : String
  , gameId : String
  , title : String
  , viewerCount: Int
  , thumbnailUrl : String
  }

liveStreams : Decoder (List LiveStream)
liveStreams =
  field "data" (list stream)

stream : Decoder LiveStream
stream =
  map6 LiveStream
    (field "id" string)
    (field "user_id" string)
    (field "game_id" string)
    (field "title" string)
    (field "viewer_count" int)
    (field "thumbnail_url" string)

{-"data":
   [
      {
         "id":"493057",
         "name":"PLAYERUNKNOWN'S BATTLEGROUNDS",
         "box_art_url":"https://static-cdn.jtvnw.net/ttv-boxart/PLAYERUNKNOWN%27S%20BATTLEGROUNDS-{width}x{height}.jpg"
      }
   ]
-}

type alias Game =
  { id : String
  , name : String
  , boxArtUrl : String
  }

games : Decoder (List Game)
games =
  field "data" (list game)

game : Decoder Game
game =
  map3 Game
    (field "id" string)
    (field "name" string)
    (field "box_art_url" string)

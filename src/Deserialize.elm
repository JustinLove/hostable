module Deserialize exposing (LiveStream, users, liveStreams)

import Json.Decode exposing (..)

{-
   "_total": 2,
   "users": [
      {
         "_id": "44322889",
         "bio": "Just a gamer playing games and chatting. :)",
         "created_at": "2013-06-03T19:12:02.580593Z",
         "display_name": "dallas",
         "logo": "https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-profile_image-1a2c906ee2c35f12-300x300.png",
         "name": "dallas",
         "type": "staff",
         "updated_at": "2017-02-09T16:32:06.784398Z"
      },
      {
         "_id": "129454141",
         "bio": null,
         "created_at": "2016-07-13T14:40:42.398257Z",
         "display_name": "dallasnchains",
         "logo": null,
         "name": "dallasnchains",
         "type": "user",
         "updated_at": "2017-02-04T14:32:38.626459Z"
      }
   ]
-}

users : Decoder (List String)
users =
  field "users" (list (field "_id" string))


{-
   "_total": 1295,
   "streams": [
   {
      "_id": 23937446096,
      "average_fps": 60,
      "channel": {
            "_id": 121059319,
            "broadcaster_language": "en",
            "created_at": "2016-04-06T04:12:40Z",
            "display_name": "MOONMOON_OW",
            "followers": 251220,
            "game": "Overwatch",
            "language": "en",
            "logo": "https://static-cdn.jtvnw.net/jtv_user_pictures/moonmoon_ow-profile_image-0fe586039bb28259-300x300.png",
            "mature": true,
            "name": "moonmoon_ow",
            "partner": true,
            "profile_banner": "https://static-cdn.jtvnw.net/jtv_user_pictures/moonmoon_ow-profile_banner-13fbfa1ba07bcd8a-480.png",
            "profile_banner_background_color": null,
            "status": "KKona where my Darryl subs at KKona",
            "updated_at": "2016-12-15T20:04:53Z",
            "url": "https://www.twitch.tv/moonmoon_ow",
            "video_banner": "https://static-cdn.jtvnw.net/jtv_user_pictures/moonmoon_ow-channel_offline_image-2b3302e20384eee8-1920x1080.png",
            "views": 9869754      }, "created_at": "2016-12-15T14:55:49Z", "delay": 0,
      "game": "Overwatch",
      "is_playlist": false,
      "preview": {
            "large": "https://static-cdn.jtvnw.net/previews-ttv/live_user_moonmoon_ow-640x360.jpg",
            "medium": "https://static-cdn.jtvnw.net/previews-ttv/live_user_moonmoon_ow-320x180.jpg",
            "small": "https://static-cdn.jtvnw.net/previews-ttv/live_user_moonmoon_ow-80x45.jpg",
            "template": "https://static-cdn.jtvnw.net/previews-ttv/live_user_moonmoon_ow-{width}x{height}.jpg"
      },
      "video_height": 720,
      "viewers": 11523
   },
      ...
   ]
-}

liveStreams : Decoder (List LiveStream)
liveStreams =
  field "streams" (list stream)

type alias LiveStream =
  { channelId : String
  , displayName : String
  , game : String
  , status : String
  , url : String
  , preview : String
  }

stream : Decoder LiveStream
stream =
  map6 LiveStream
    (field "channel" (field "_id" (map toString int)))
    (field "channel" (field "display_name" string))
    (field "game" string)
    (field "channel" (field "status" string))
    (field "channel" (field "url" string))
    (field "preview" (field "medium" string))

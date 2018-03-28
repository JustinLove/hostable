import Twitch.Deserialize exposing (User, users, sampleUser)

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)
import Json.Decode

main : Html msg
main =
  runAll all

all : Test
all = describe "Deserialize"
  [ it "deserializes sample user" <|
    eql
      (Json.Decode.decodeString users sampleUser)
      (Ok [
        ( User
          "44322889"
          "dallas"
          "dallas"
          "staff"
          ""
          "Just a gamer playing games and chatting. :)"
          "https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-profile_image-1a2c906ee2c35f12-300x300.png"
          "https://static-cdn.jtvnw.net/jtv_user_pictures/dallas-channel_offline_image-1a2c906ee2c35f12-1920x1080.png"
          191836881
          (Just "login@provider.com")
        )])
  ]

roundTrips : (a -> Json.Decode.Value) -> Json.Decode.Decoder a -> a -> Expectation.Expectation
roundTrips encoder decoder value =
  eql
    (Ok value)
    (value |> encoder |> Json.Decode.decodeValue decoder)

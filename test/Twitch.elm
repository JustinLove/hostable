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
    decodes <| Json.Decode.decodeString users sampleUser
  ]

decodes : Result String a -> Expectation.Expectation
decodes result =
  isTrue (case result of
    Ok _ -> True
    Err _ -> False)

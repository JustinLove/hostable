import Persist exposing (Persist, User, Game)
import Persist.Decode
import Persist.Encode

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)
import Json.Decode

main : Html msg
main =
  runAll all

all : Test
all = describe "serialization"
  [ it "roundtrips user" <| roundTrips
    Persist.Encode.user
    Persist.Decode.user
    (User "1" "name")
  , it "roundtrips game" <| roundTrips
    Persist.Encode.game
    Persist.Decode.game
    (Game "1" "name" "http://example.com")
  , it "roundtrips persist" <| roundTrips
    Persist.Encode.persist
    Persist.Decode.persist
    ( Persist
      [ (User "1" "name") ]
      [ (Game "1" "name" "http://example.com") ]
    )
  ]

roundTrips : (a -> Json.Decode.Value) -> Json.Decode.Decoder a -> a -> Expectation.Expectation
roundTrips encoder decoder value =
  eql
    (Ok value)
    (value |> encoder |> Json.Decode.decodeValue decoder)

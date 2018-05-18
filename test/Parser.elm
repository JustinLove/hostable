import Twitch.Parse exposing (..)

import Expectation exposing (eql, isTrue, isFalse)
import Test exposing (it, describe, Test)
import Runner exposing (runAll)

import Html exposing (Html)
import Parser exposing ((|.))

main : Html msg
main =
  runAll all

all : Test
all = describe "parsering"
  [ describe "builtin functions"
    [ it "int" <|
      eql (Ok 11) (Parser.run Parser.int "11")
    , it "does not handle suffixed int" <|
      case (Parser.run Parser.int "11h") of
        Err err -> let _ = Debug.log "err" err in isTrue True
        Ok _ -> isTrue False
    ]
  , describe "custom functions"
    [ it "int" <|
      eql (Ok 11) (Parser.run int "11")
    , it "suffixed int" <|
      eql (Ok 11) (Parser.run int "11h")
    , it "suffixed int and suffix" <|
      eql (Ok 11) (Parser.run (int |. Parser.symbol "h") "11h")
    , it "3 element duration" <|
      eql
        (Ok ((60 * 60 * 11 + 60 * 2 + 3) * 1000))
        (Parser.run (duration) "11h02m3s")
    , it "2 element duration" <|
      eql
        (Ok ((60 * 60 * 0 + 60 * 2 + 3) * 1000))
        (Parser.run (duration) "02m3s")
    , it "1 element duration" <|
      eql
        (Ok ((60 * 60 * 0 + 60 * 0 + 3) * 1000))
        (Parser.run (duration) "3s")
    , it "ignores invalid input" <|
      case (Parser.run duration "3x") of
        Err err -> let _ = Debug.log "err" err in isTrue True
        Ok _ -> isTrue False
    , it "ignores invalid input" <|
      case (Parser.run duration "s") of
        Err err -> let _ = Debug.log "err" err in isTrue True
        Ok _ -> isTrue False
    ]
  ]

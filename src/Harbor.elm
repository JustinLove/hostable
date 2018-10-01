port module Harbor exposing (..)

import Json.Decode

port select : String -> Cmd msg
port read : Json.Decode.Value -> Cmd msg
port fileContents : (String -> msg) -> Sub msg

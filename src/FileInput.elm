port module FileInput exposing (Files, read, fileContents, targetFiles)

import Json.Decode

type alias Files = Json.Decode.Value

port read : Files -> Cmd msg
port fileContents : (String -> msg) -> Sub msg

targetFiles : (Files -> msg) -> Json.Decode.Decoder msg
targetFiles tagger =
  (Json.Decode.at ["target", "files"] Json.Decode.value)
    |> Json.Decode.map tagger
    --|> Json.Decode.map (Debug.log "files")

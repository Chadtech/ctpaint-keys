module Tests exposing (encoderTests)

import Data.Keys as Keys
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


encoderTests : Test
encoderTests =
    [ test "encoding empty list is empty list json" <|
        \() ->
            []
                |> Keys.encodeConfig
                |> Encode.encode 0
                |> Expect.equal "[]"
    , test "Encoding default config looks right" <|
        \() ->
            Keys.defaultConfig
                |> Keys.encodeConfig
                |> Encode.encode 0
                |> Expect.equal defaultConfigStr
    , test "Encoding first quick key in default config looks right" <|
        \() ->
            Keys.defaultConfig
                |> List.head
                |> Maybe.map List.singleton
                |> Maybe.map Keys.encodeConfig
                |> Maybe.map (Encode.encode 0)
                |> Expect.equal (Just swatchesTurnLeftStr)
    , test "encoding and decoding are reversible operations" <|
        \() ->
            Keys.defaultConfig
                |> Keys.encodeConfig
                |> Decode.decodeValue Keys.configDecoder
                |> Expect.equal (Ok Keys.defaultConfig)
    ]
        |> describe "encoders"


defaultConfigStr : String
defaultConfigStr =
    "[{\"quick-key\":{\"direction\":\"down\",\"key\":50,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"swatches-quick-turn-left\"},{\"quick-key\":{\"direction\":\"down\",\"key\":51,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"swatches-quick-turn-down\"},{\"quick-key\":{\"direction\":\"down\",\"key\":52,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"swatches-quick-turn-right\"},{\"quick-key\":{\"direction\":\"down\",\"key\":49,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"swatches-turn-left\"},{\"quick-key\":{\"direction\":\"up\",\"key\":50,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"revert-quick-turn-left\"},{\"quick-key\":{\"direction\":\"up\",\"key\":51,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"revert-quick-turn-down\"},{\"quick-key\":{\"direction\":\"up\",\"key\":52,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"revert-quick-turn-right\"},{\"quick-key\":{\"direction\":\"down\",\"key\":53,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"swatches-turn-right\"},{\"quick-key\":{\"direction\":\"down\",\"key\":80,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-pencil\"},{\"quick-key\":{\"direction\":\"down\",\"key\":72,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-hand\"},{\"quick-key\":{\"direction\":\"down\",\"key\":83,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-select\"},{\"quick-key\":{\"direction\":\"down\",\"key\":71,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-fill\"},{\"quick-key\":{\"direction\":\"down\",\"key\":73,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-sample\"},{\"quick-key\":{\"direction\":\"down\",\"key\":76,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-line\"},{\"quick-key\":{\"direction\":\"down\",\"key\":85,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-rectangle\"},{\"quick-key\":{\"direction\":\"down\",\"key\":74,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"set-tool-to-rectangle-filled\"},{\"quick-key\":{\"direction\":\"down\",\"key\":90,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"undo\"},{\"quick-key\":{\"direction\":\"down\",\"key\":89,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"redo\"},{\"quick-key\":{\"direction\":\"down\",\"key\":67,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"copy\"},{\"quick-key\":{\"direction\":\"down\",\"key\":88,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"cut\"},{\"quick-key\":{\"direction\":\"down\",\"key\":86,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"paste\"},{\"quick-key\":{\"direction\":\"down\",\"key\":65,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"select-all\"},{\"quick-key\":{\"direction\":\"down\",\"key\":61,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"zoom-in\"},{\"quick-key\":{\"direction\":\"down\",\"key\":189,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"zoom-out\"},{\"quick-key\":{\"direction\":\"down\",\"key\":192,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"toggle-minimap\"},{\"quick-key\":{\"direction\":\"down\",\"key\":68,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"init-download\"},{\"quick-key\":{\"direction\":\"down\",\"key\":73,\"cmd-key-state\":\"down\",\"shift-state\":\"up\"},\"cmd\":\"init-import\"},{\"quick-key\":{\"direction\":\"down\",\"key\":68,\"cmd-key-state\":\"down\",\"shift-state\":\"down\"},\"cmd\":\"init-scale\"},{\"quick-key\":{\"direction\":\"down\",\"key\":84,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"init-text\"},{\"quick-key\":{\"direction\":\"down\",\"key\":82,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"init-replace-color\"},{\"quick-key\":{\"direction\":\"down\",\"key\":9,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"switch-gallery-view\"},{\"quick-key\":{\"direction\":\"down\",\"key\":8,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"delete\"},{\"quick-key\":{\"direction\":\"down\",\"key\":69,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"toggle-color-picker\"},{\"quick-key\":{\"direction\":\"down\",\"key\":72,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"flip-horizontal\"},{\"quick-key\":{\"direction\":\"down\",\"key\":86,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"flip-vertical\"},{\"quick-key\":{\"direction\":\"down\",\"key\":82,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"rotate90\"},{\"quick-key\":{\"direction\":\"down\",\"key\":70,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"rotate180\"},{\"quick-key\":{\"direction\":\"down\",\"key\":69,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"rotate270\"},{\"quick-key\":{\"direction\":\"down\",\"key\":73,\"cmd-key-state\":\"up\",\"shift-state\":\"down\"},\"cmd\":\"invert-colors\"}]"


swatchesTurnLeftStr : String
swatchesTurnLeftStr =
    "[{\"quick-key\":{\"direction\":\"down\",\"key\":50,\"cmd-key-state\":\"up\",\"shift-state\":\"up\"},\"cmd\":\"swatches-quick-turn-left\"}]"

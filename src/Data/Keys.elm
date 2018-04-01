module Data.Keys
    exposing
        ( Cmd(..)
        , Config
        , Event
        , QuickKey
        , allCmds
        , configDecoder
        , defaultConfig
        , encodeConfig
        , eventDecoder
        , initCmdLookUp
        , initQuickKeysLookUp
        )

{-| Keys Module for CtPaint Project


# Types

@docs Event, Config, Cmd, allCmds, QuickKey


# Default

@docs defaultConfig


# Decoders

@docs eventDecoder, configDecoder


# Encoders

@docs encodeConfig


# Init

@docs initCmdLookUp, initQuickKeysLookUp

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline
    exposing
        ( decode
        , required
        )
import Json.Encode as Encode exposing (Value)
import Keyboard exposing (KeyCode)
import Keyboard.Extra.Browser exposing (Browser, Key(..))
import String.Extra
import Tuple.Infix exposing ((:=))


-- TYPES --


{-| Events represent key board events
-}
type alias Event =
    { code : KeyCode
    , meta : Bool
    , ctrl : Bool
    , shift : Bool
    , direction : Direction
    }


{-| The direction union type represents whether the key event was a down press or a key release
-}
type Direction
    = Up
    | Down


{-| Cmds are things particular key strokes can execute
-}
type Cmd
    = SwatchesTurnLeft
    | SwatchesTurnRight
    | SwatchesQuickTurnLeft
    | RevertQuickTurnLeft
    | SwatchesQuickTurnRight
    | RevertQuickTurnRight
    | SwatchesQuickTurnDown
    | RevertQuickTurnDown
    | SetToolToPencil
    | SetToolToHand
    | SetToolToSelect
    | SetToolToFill
    | SetToolToEraser
    | SetToolToSample
    | SetToolToLine
    | SetToolToRectangle
    | SetToolToRectangleFilled
    | Undo
    | Redo
    | Cut
    | Copy
    | SelectAll
    | Paste
    | ZoomIn
    | ZoomOut
    | InitDownload
    | InitImport
    | InitScale
    | InitText
    | InitReplaceColor
    | ToggleColorPicker
    | SwitchGalleryView
    | ToggleMinimap
    | Delete
    | FlipHorizontal
    | FlipVertical
    | Rotate90
    | Rotate180
    | Rotate270
    | InvertColors
    | Save
    | SetTransparency
    | InitUpload
    | InitResize
    | NoCmd


{-| A list of all the cmds
-}
allCmds : List Cmd
allCmds =
    [ SwatchesTurnLeft
    , SwatchesTurnRight
    , SwatchesQuickTurnLeft
    , RevertQuickTurnLeft
    , SwatchesQuickTurnRight
    , RevertQuickTurnRight
    , SwatchesQuickTurnDown
    , RevertQuickTurnDown
    , SetToolToPencil
    , SetToolToHand
    , SetToolToSelect
    , SetToolToFill
    , SetToolToSample
    , SetToolToLine
    , SetToolToRectangle
    , SetToolToRectangleFilled
    , Undo
    , Redo
    , Cut
    , Copy
    , SelectAll
    , Paste
    , ZoomIn
    , ZoomOut
    , InitDownload
    , InitImport
    , InitScale
    , InitText
    , InitReplaceColor
    , ToggleColorPicker
    , SwitchGalleryView
    , ToggleMinimap
    , Delete
    , FlipHorizontal
    , FlipVertical
    , Rotate90
    , Rotate180
    , Rotate270
    , InvertColors
    , Save
    , SetTransparency
    , NoCmd
    ]


type CmdKeyState
    = CmdKeyIsDown
    | CmdKeyIsUp


type ShiftState
    = ShiftIsDown
    | ShiftIsUp


{-| Certain keys execute certain cmds. Those key combinations need to be represented to the user, the QuickKey type represents that
-}
type alias QuickKey =
    ( Direction, Key, CmdKeyState, ShiftState )


{-| A config represents the full specification of keys a user presses to issue which cmds
-}
type alias Config =
    List ( QuickKey, Cmd )


{-| Users must start with some kind of config. This is the default one they start off with
-}
defaultConfig : List ( QuickKey, Cmd )
defaultConfig =
    [ ( Down, Number2, CmdKeyIsUp, ShiftIsUp ) := SwatchesQuickTurnLeft
    , ( Down, Number3, CmdKeyIsUp, ShiftIsUp ) := SwatchesQuickTurnDown
    , ( Down, Number4, CmdKeyIsUp, ShiftIsUp ) := SwatchesQuickTurnRight
    , ( Down, Number1, CmdKeyIsUp, ShiftIsUp ) := SwatchesTurnLeft
    , ( Up, Number2, CmdKeyIsUp, ShiftIsUp ) := RevertQuickTurnLeft
    , ( Up, Number3, CmdKeyIsUp, ShiftIsUp ) := RevertQuickTurnDown
    , ( Up, Number4, CmdKeyIsUp, ShiftIsUp ) := RevertQuickTurnRight
    , ( Down, Number5, CmdKeyIsUp, ShiftIsUp ) := SwatchesTurnRight
    , ( Down, CharP, CmdKeyIsUp, ShiftIsUp ) := SetToolToPencil
    , ( Down, CharH, CmdKeyIsUp, ShiftIsUp ) := SetToolToHand
    , ( Down, CharS, CmdKeyIsUp, ShiftIsUp ) := SetToolToSelect
    , ( Down, CharG, CmdKeyIsUp, ShiftIsUp ) := SetToolToFill
    , ( Down, CharE, CmdKeyIsUp, ShiftIsUp ) := SetToolToEraser
    , ( Down, CharI, CmdKeyIsUp, ShiftIsUp ) := SetToolToSample
    , ( Down, CharL, CmdKeyIsUp, ShiftIsUp ) := SetToolToLine
    , ( Down, CharU, CmdKeyIsUp, ShiftIsUp ) := SetToolToRectangle
    , ( Down, CharJ, CmdKeyIsUp, ShiftIsUp ) := SetToolToRectangleFilled
    , ( Down, CharZ, CmdKeyIsDown, ShiftIsUp ) := Undo
    , ( Down, CharY, CmdKeyIsDown, ShiftIsUp ) := Redo
    , ( Down, CharC, CmdKeyIsDown, ShiftIsUp ) := Copy
    , ( Down, CharX, CmdKeyIsDown, ShiftIsUp ) := Cut
    , ( Down, CharV, CmdKeyIsDown, ShiftIsUp ) := Paste
    , ( Down, CharA, CmdKeyIsDown, ShiftIsUp ) := SelectAll
    , ( Down, Equals, CmdKeyIsUp, ShiftIsUp ) := ZoomIn
    , ( Down, Minus, CmdKeyIsUp, ShiftIsUp ) := ZoomOut
    , ( Down, BackQuote, CmdKeyIsUp, ShiftIsUp ) := ToggleMinimap
    , ( Down, CharD, CmdKeyIsUp, ShiftIsDown ) := InitDownload
    , ( Down, CharI, CmdKeyIsDown, ShiftIsUp ) := InitImport
    , ( Down, CharD, CmdKeyIsDown, ShiftIsDown ) := InitScale
    , ( Down, CharT, CmdKeyIsUp, ShiftIsUp ) := InitText
    , ( Down, CharR, CmdKeyIsUp, ShiftIsUp ) := InitReplaceColor
    , ( Down, Tab, CmdKeyIsUp, ShiftIsUp ) := SwitchGalleryView
    , ( Down, BackSpace, CmdKeyIsUp, ShiftIsUp ) := Delete
    , ( Down, CharE, CmdKeyIsDown, ShiftIsDown ) := ToggleColorPicker
    , ( Down, CharH, CmdKeyIsUp, ShiftIsDown ) := FlipHorizontal
    , ( Down, CharV, CmdKeyIsUp, ShiftIsDown ) := FlipVertical
    , ( Down, CharR, CmdKeyIsUp, ShiftIsDown ) := Rotate90
    , ( Down, CharF, CmdKeyIsUp, ShiftIsDown ) := Rotate180
    , ( Down, CharE, CmdKeyIsUp, ShiftIsDown ) := Rotate270
    , ( Down, CharI, CmdKeyIsUp, ShiftIsDown ) := InvertColors
    , ( Down, Space, CmdKeyIsUp, ShiftIsUp ) := SetTransparency
    , ( Down, CharO, CmdKeyIsDown, ShiftIsUp ) := InitUpload
    , ( Down, CharR, CmdKeyIsDown, ShiftIsDown ) := InitResize
    ]



-- Cmd Look Up --


{-| The dictionary used to map key events to cmds
-}
initCmdLookUp : Browser -> Config -> Dict String Cmd
initCmdLookUp browser =
    List.map (Tuple.mapFirst (quickKeyToString browser))
        >> Dict.fromList


{-| The dictionary used to map cmds to key arrangements (Undo -> "cmd + z")
-}
initQuickKeysLookUp : Browser -> Config -> Bool -> Dict String String
initQuickKeysLookUp browser config isMac =
    config
        |> List.map (quickKeyLookUp browser isMac)
        |> Dict.fromList


quickKeyLookUp : Browser -> Bool -> ( QuickKey, Cmd ) -> ( String, String )
quickKeyLookUp browser isMac ( ( _, key, cmdKey, shift ), command ) =
    let
        commandStr =
            toString command

        cmdKeyStr =
            case ( cmdKey, isMac ) of
                ( CmdKeyIsDown, True ) ->
                    "Cmd + "

                ( CmdKeyIsDown, False ) ->
                    "Ctrl + "

                _ ->
                    ""

        shiftStr =
            if shift == ShiftIsDown then
                "Shift + "
            else
                ""

        keyStr =
            key
                |> Keyboard.Extra.Browser.toCode browser
                |> keyCodeToString browser
    in
    ( commandStr, cmdKeyStr ++ shiftStr ++ keyStr )


quickKeyToString : Browser -> QuickKey -> String
quickKeyToString browser ( direction, key, cmd, shift ) =
    let
        code =
            key
                |> Keyboard.Extra.Browser.toCode browser
                |> toString

        cmdStr =
            cmd
                == CmdKeyIsDown
                |> toString

        shiftStr =
            shift
                == ShiftIsDown
                |> toString
    in
    shiftStr ++ cmdStr ++ code ++ toString direction


keyCodeToString : Browser -> KeyCode -> String
keyCodeToString browser key =
    case Keyboard.Extra.Browser.fromCode browser key of
        Control ->
            "Ctrl"

        QuestionMark ->
            "?"

        Equals ->
            "="

        Minus ->
            "-"

        Semicolon ->
            ";"

        Super ->
            "Cmd"

        Asterisk ->
            "*"

        Comma ->
            ","

        Dollar ->
            "$"

        BackQuote ->
            "`"

        other ->
            let
                otherAsStr =
                    toString other

                isChar =
                    String.left 4 otherAsStr == "Char"

                isNumber =
                    String.left 6 otherAsStr == "Number"
            in
            case ( isChar, isNumber ) of
                ( True, _ ) ->
                    String.right 1 otherAsStr

                ( _, True ) ->
                    String.right 1 otherAsStr

                _ ->
                    otherAsStr



-- ENCODER --


{-| Turn the config into json
-}
encodeConfig : Browser -> Config -> Value
encodeConfig browser =
    List.map (encodeCmdPair browser) >> Encode.list


encodeCmdPair : Browser -> ( QuickKey, Cmd ) -> Value
encodeCmdPair browser ( quickKey, cmd ) =
    [ "quick-key" := encodeQuickKey browser quickKey
    , "cmd" := simpleEncode cmd
    ]
        |> Encode.object


encodeQuickKey : Browser -> QuickKey -> Value
encodeQuickKey browser ( direction, key, cmdKeyState, shiftState ) =
    [ "direction" := simpleEncode direction
    , "key" := encodeKey browser key
    , "cmd-key-state" := encodeCmdKeyState cmdKeyState
    , "shift-state" := encodeShiftState shiftState
    ]
        |> Encode.object


encodeKey : Browser -> Key -> Value
encodeKey browser =
    Keyboard.Extra.Browser.toCode browser >> Encode.int


encodeCmdKeyState : CmdKeyState -> Value
encodeCmdKeyState cmdKeyState =
    case cmdKeyState of
        CmdKeyIsDown ->
            simpleEncode Down

        CmdKeyIsUp ->
            simpleEncode Up


encodeShiftState : ShiftState -> Value
encodeShiftState shiftState =
    case shiftState of
        ShiftIsDown ->
            simpleEncode Down

        ShiftIsUp ->
            simpleEncode Up


simpleEncode : a -> Value
simpleEncode =
    toString >> dasherize >> Encode.string


dasherize : String -> String
dasherize =
    String.Extra.dasherize >> String.dropLeft 1



-- DECODER --


{-| The decoder for key events
-}
eventDecoder : Decoder Event
eventDecoder =
    decode Event
        |> required "keyCode" Decode.int
        |> required "meta" Decode.bool
        |> required "ctrl" Decode.bool
        |> required "shift" Decode.bool
        |> required "direction" directionDecoder


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.andThen toDirection


toDirection : String -> Decoder Direction
toDirection str =
    case str of
        "up" ->
            Decode.succeed Up

        "down" ->
            Decode.succeed Down

        _ ->
            Decode.fail "Direction not up or down"


{-| Turn json into a key config using the config decoder
-}
configDecoder : Browser -> Decoder Config
configDecoder =
    Decode.list << cmdPairDecoder


cmdPairDecoder : Browser -> Decoder ( QuickKey, Cmd )
cmdPairDecoder browser =
    decode (,)
        |> required "quick-key" (quickKeyDecoder browser)
        |> required "cmd" cmdDecoder


quickKeyDecoder : Browser -> Decoder QuickKey
quickKeyDecoder browser =
    decode (,,,)
        |> required "direction" directionDecoder
        |> required "key" (keyDecoder browser)
        |> required "cmd-key-state" cmdKeyStateDecoder
        |> required "shift-state" shiftStateDecoder


cmdDecoder : Decoder Cmd
cmdDecoder =
    Decode.string
        |> Decode.andThen toCmd


toCmd : String -> Decoder Cmd
toCmd str =
    case str of
        "swatches-turn-left" ->
            Decode.succeed SwatchesTurnLeft

        "swatches-turn-right" ->
            Decode.succeed SwatchesTurnRight

        "swatches-quick-turn-left" ->
            Decode.succeed SwatchesQuickTurnLeft

        "revert-quick-turn-left" ->
            Decode.succeed RevertQuickTurnLeft

        "swatches-quick-turn-right" ->
            Decode.succeed SwatchesQuickTurnRight

        "revert-quick-turn-right" ->
            Decode.succeed RevertQuickTurnRight

        "swatches-quick-turn-down" ->
            Decode.succeed SwatchesQuickTurnDown

        "revert-quick-turn-down" ->
            Decode.succeed RevertQuickTurnDown

        "set-tool-to-pencil" ->
            Decode.succeed SetToolToPencil

        "set-tool-to-hand" ->
            Decode.succeed SetToolToHand

        "set-tool-to-select" ->
            Decode.succeed SetToolToSelect

        "set-tool-to-fill" ->
            Decode.succeed SetToolToFill

        "set-tool-to-sample" ->
            Decode.succeed SetToolToSample

        "set-tool-to-line" ->
            Decode.succeed SetToolToLine

        "set-tool-to-rectangle" ->
            Decode.succeed SetToolToRectangle

        "set-tool-to-rectangle-filled" ->
            Decode.succeed SetToolToRectangleFilled

        "undo" ->
            Decode.succeed Undo

        "redo" ->
            Decode.succeed Redo

        "cut" ->
            Decode.succeed Cut

        "copy" ->
            Decode.succeed Copy

        "select-all" ->
            Decode.succeed SelectAll

        "paste" ->
            Decode.succeed Paste

        "zoom-in" ->
            Decode.succeed ZoomIn

        "zoom-out" ->
            Decode.succeed ZoomOut

        "init-download" ->
            Decode.succeed InitDownload

        "init-import" ->
            Decode.succeed InitImport

        "init-scale" ->
            Decode.succeed InitScale

        "init-text" ->
            Decode.succeed InitText

        "init-replace-color" ->
            Decode.succeed InitReplaceColor

        "init-reszie" ->
            Decode.succeed InitResize

        "toggle-color-picker" ->
            Decode.succeed ToggleColorPicker

        "switch-gallery-view" ->
            Decode.succeed SwitchGalleryView

        "toggle-minimap" ->
            Decode.succeed ToggleMinimap

        "delete" ->
            Decode.succeed Delete

        "flip-horizontal" ->
            Decode.succeed FlipHorizontal

        "flip-vertical" ->
            Decode.succeed FlipVertical

        "rotate90" ->
            Decode.succeed Rotate90

        "rotate180" ->
            Decode.succeed Rotate180

        "rotate270" ->
            Decode.succeed Rotate270

        "invert-colors" ->
            Decode.succeed InvertColors

        "save" ->
            Decode.succeed Save

        _ ->
            Decode.fail ("unrecognized cmd : " ++ str)


cmdKeyStateDecoder : Decoder CmdKeyState
cmdKeyStateDecoder =
    Decode.map toCmdKeyState directionDecoder


shiftStateDecoder : Decoder ShiftState
shiftStateDecoder =
    Decode.map toShiftState directionDecoder


toShiftState : Direction -> ShiftState
toShiftState direction =
    case direction of
        Up ->
            ShiftIsUp

        Down ->
            ShiftIsDown


toCmdKeyState : Direction -> CmdKeyState
toCmdKeyState direction =
    case direction of
        Up ->
            CmdKeyIsUp

        Down ->
            CmdKeyIsDown


keyDecoder : Browser -> Decoder Key
keyDecoder browser =
    Decode.map
        (Keyboard.Extra.Browser.fromCode browser)
        Decode.int

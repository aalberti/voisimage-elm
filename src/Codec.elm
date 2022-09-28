module Codec exposing (..)

import Array
import Game exposing (Cell, Hint(..), State(..))
import Grid exposing (Grid, Row)
import Json.Decode as Decode exposing (Error)
import Json.Encode as Encode

encode : Grid Cell -> Encode.Value
encode g = Array.toList g |> Encode.list encodeRow

encodeRow : Row Cell -> Encode.Value
encodeRow r = Array.toList r |> Encode.list encodeCell

encodeCell : Cell -> Encode.Value
encodeCell c = Encode.object
    [ ("state", encodeState c.state )
    , ("hint", encodeHint c.hint)
    ]

encodeState : State -> Encode.Value
encodeState s = case s of
    Marked -> Encode.string "marked"
    Unmarked -> Encode.string "unmarked"
    Unknown -> Encode.string "unknown"

encodeHint : Hint -> Encode.Value
encodeHint h = case h of
    NoHint -> Encode.string ""
    CellsToMark number -> String.fromInt number |> Encode.string

decode : Encode.Value -> Result Error (Grid Cell)
decode encoded = case Decode.decodeValue grid encoded of
    Ok value -> Grid.from value |> Ok
    Err error -> Err error

grid : Decode.Decoder (List (List Cell))
grid = Decode.list row

row : Decode.Decoder (List Cell)
row = Decode.list cell

cell : Decode.Decoder Cell
cell = Decode.map2 Cell
    (Decode.field "state" state)
    (Decode.field "hint" hint)

state: Decode.Decoder State
state = Decode.string |> Decode.andThen stateType

stateType: String -> Decode.Decoder State
stateType t = case t of
    "marked" -> Decode.succeed Marked
    "unmarked" -> Decode.succeed Unmarked
    _ -> Decode.succeed Unknown

hint : Decode.Decoder Hint
hint = Decode.string |> Decode.andThen hintType

hintType: String -> Decode.Decoder Hint
hintType t = case t of
    "" -> Decode.succeed NoHint
    _ -> Decode.succeed (String.toInt t |> Maybe.map CellsToMark |> Maybe.withDefault NoHint)

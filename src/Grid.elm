module Grid exposing (..)

import Array exposing (Array)

type alias Coordinates =
    { x: Int
    , y: Int
    }
type alias Grid a = Array (Row a)
type alias Row a = Array a

from : List (List a) -> Grid a
from rows = List.map (Array.fromList) rows |> Array.fromList

indexedRowsMap : (Int -> Row a -> b) -> Grid a -> List b
indexedRowsMap f grid = Array.indexedMap f grid |> Array.toList

indexedCellsMap : (Int -> a -> b) -> Row a -> List b
indexedCellsMap f row = Array.indexedMap f row |> Array.toList

update : (a -> a) -> Grid a -> Coordinates -> Grid a
update f grid coordinates = get grid coordinates
    |> Maybe.map (f)
    |> Maybe.map (set grid coordinates)
    |> Maybe.withDefault grid

count : (a -> Bool) -> Grid a -> Int
count predicate grid = Array.toList grid
    |> List.map (countCells predicate)
    |> List.sum

indexedCount : (Coordinates -> a -> Bool) -> Grid a -> Int
indexedCount predicate grid =
    Array.indexedMap (indexedCellsCount predicate) grid
        |> Array.toList
        |> List.sum

indexedCellsCount : (Coordinates -> a -> Bool) -> Int -> Row a -> Int
indexedCellsCount predicate x row =
    Array.indexedMap (\y a -> predicate {x = x, y = y} a) row
        |> Array.filter (\a -> a)
        |> Array.toList
        |> List.length

countCells : (a -> Bool) -> Row a -> Int
countCells predicate row = Array.toList row
    |> List.filter predicate
    |> List.length

rowCell : Int -> Row a -> Maybe a
rowCell x row = Array.get x row

get: Grid a -> Coordinates -> Maybe a
get grid coordinates = Array.get coordinates.y grid |> Maybe.andThen (rowCell coordinates.x)

set : Grid a -> Coordinates -> a -> Grid a
set grid coordinates cell = Array.get coordinates.y grid
    |> Maybe.map (setCellToRow coordinates.x cell)
    |> Maybe.map (setRowToGrid coordinates.y grid)
    |> Maybe.withDefault grid

setRowToGrid : Int -> Grid a -> Row a -> Grid a
setRowToGrid y grid row = Array.set y row grid

setCellToRow : Int -> a -> Row a -> Row a
setCellToRow x cell row = Array.set x cell row

neighbours : Grid a -> Coordinates -> Grid a
neighbours grid coordinates = from
    (
        [ line grid coordinates (coordinates.y - 1)
        , line grid coordinates coordinates.y
        , line grid coordinates (coordinates.y + 1)
        ]
        |> List.filter (\list -> not (List.isEmpty list))
    )

line : Grid a -> Coordinates -> Int -> List a
line grid coordinates index =
    Array.get index grid
        |> Maybe.withDefault (Array.fromList [])
        |> subArray (coordinates.x - 1) (coordinates.x + 1)
        |> Array.toList

subArray : Int -> Int -> Array a -> Array a
subArray start end array =
    let
        s = if start < 0 then
                0
            else
                start
        e = if end < Array.length array then
                end
            else
                (Array.length array) - 1
    in
        Array.slice s (e+1) array
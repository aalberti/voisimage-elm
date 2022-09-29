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
count predicate grid =
    let
        countCells : (a -> Bool) -> Row a -> Int
        countCells p row = Array.toList row
            |> List.filter p
            |> List.length
    in
        Array.toList grid
            |> List.map (countCells predicate)
            |> List.sum

indexedCount : (Coordinates -> a -> Bool) -> Grid a -> Int
indexedCount predicate grid =
    let
        indexedCellsCount : (Coordinates -> a -> Bool) -> Int -> Row a -> Int
        indexedCellsCount p y row =
            Array.indexedMap (\x a -> p {x = x, y = y} a) row
                |> Array.filter (\a -> a)
                |> Array.toList
                |> List.length
    in
        Array.indexedMap (indexedCellsCount predicate) grid
            |> Array.toList
            |> List.sum

get: Grid a -> Coordinates -> Maybe a
get grid coordinates =
    let
        rowCell : Int -> Row a -> Maybe a
        rowCell x row = Array.get x row
    in
        Array.get coordinates.y grid |> Maybe.andThen (rowCell coordinates.x)

set : Grid a -> Coordinates -> a -> Grid a
set grid coordinates cell =
    let
        setRowToGrid : Int -> Grid a -> Row a -> Grid a
        setRowToGrid y g row = Array.set y row g

        setCellToRow : Int -> a -> Row a -> Row a
        setCellToRow x c row = Array.set x c row
    in
        Array.get coordinates.y grid
            |> Maybe.map (setCellToRow coordinates.x cell)
            |> Maybe.map (setRowToGrid coordinates.y grid)
            |> Maybe.withDefault grid

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
    let
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
    in
        Array.get index grid
            |> Maybe.withDefault (Array.fromList [])
            |> subArray (coordinates.x - 1) (coordinates.x + 1)
            |> Array.toList

replaceAll : (a -> a) -> Grid a -> Grid a
replaceAll map grid = Array.map (\row -> Array.map map row) grid

indexedReplaceAll : (Coordinates -> a -> a) -> Grid a -> Grid a
indexedReplaceAll cellMap grid =
    let
        rowIndexReplaceAll : (Coordinates -> a -> a) -> Int -> Row a -> Row a
        rowIndexReplaceAll cMap y row = Array.indexedMap (\x cell -> cMap {x=x,y=y} cell) row
    in
        Array.indexedMap (rowIndexReplaceAll cellMap) grid

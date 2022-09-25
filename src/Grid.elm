module Grid exposing (Grid, Coordinates, Row, from, get, indexedRowsMap, indexedCellsMap, update)

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

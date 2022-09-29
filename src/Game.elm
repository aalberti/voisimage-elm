module Game exposing (..)

import Grid exposing (Coordinates, Grid)
import Stack exposing (Stack)

type alias Game =
    { grid: Grid Cell
    , toUndo: Stack GameRef
    , toRedo: Stack GameRef
    }
type GameRef = GameRef Game

type Hint = NoHint | CellsToMark Int
type State = Marked | Unmarked | Unknown
type Help = NoHelp | Error
type alias Cell =
    { state: State
    , hint: Hint
    , help: Help
    }
type alias GridSize = { width: Int, height: Int }

fromSize: GridSize -> Game
fromSize {width, height} =
    { grid = Grid.from(List.repeat height (List.repeat width { state = Unknown, hint = NoHint, help = NoHelp }))
    , toUndo = Stack.empty
    , toRedo = Stack.empty
    }

undo : Game -> Game
undo game = case Stack.pop game.toUndo of
    Just (GameRef previous) -> { previous | toRedo = Stack.push game.toRedo (GameRef game) }
    Nothing -> game

redo : Game -> Game
redo game = case Stack.pop game.toRedo of
    Just (GameRef next) -> next
    Nothing -> game

toggle : Game -> Coordinates -> Game
toggle game coordinates = updateGrid game (Grid.update toggleCell game.grid coordinates)
    |> clearHelp

updateHint : Game -> Coordinates -> String -> Game
updateHint game coordinates hint =
    updateGrid game (Grid.update (updateCellHint hint) game.grid coordinates)

updateCellHint : String -> Cell -> Cell
updateCellHint hint cell =
    { cell
    | hint = String.toInt hint
        |> Maybe.map CellsToMark
        |> Maybe.withDefault NoHint
    }

toggleCell: Cell -> Cell
toggleCell cell = case cell.state of
    Unknown -> markCell cell
    Marked -> unmarkCell cell
    Unmarked -> clearCell cell

isOver : Game -> Bool
isOver game = case Grid.count isUnknown game.grid of
    0 -> (Grid.indexedCount (isHintUnverified game.grid) game.grid) == 0
    _ -> False

isUnknown : Cell -> Bool
isUnknown cell = case cell.state of
    Unknown -> True
    _ -> False

isHintUnverified : Grid Cell -> Coordinates -> Cell -> Bool
isHintUnverified grid coordinates cell = case cell.hint of
    NoHint -> False
    CellsToMark cellsToMark -> (numberOfMarkedNeighbours grid coordinates) /= cellsToMark

numberOfMarkedNeighbours : Grid Cell -> Coordinates -> Int
numberOfMarkedNeighbours grid coordinates =
    Grid.neighbours grid coordinates
        |> Grid.count isCellMarked

isCellMarked : Cell -> Bool
isCellMarked cell = case cell.state of
    Marked -> True
    _ -> False

updateGrid : Game -> Grid Cell -> Game
updateGrid game grid =
    { game
    | grid = grid
    , toUndo = Stack.push game.toUndo (GameRef game)
    , toRedo = Stack.empty
    }

markCell : Cell -> Cell
markCell cell = { cell | state = Marked }

unmarkCell  : Cell -> Cell
unmarkCell cell = { cell | state = Unmarked }

clearCell : Cell -> Cell
clearCell cell = { cell | state = Unknown }

clearMarks : Game -> Game
clearMarks game =
    { game
    | grid = Grid.replaceAll clearMark game.grid
    , toUndo = Stack.push game.toUndo (GameRef game)
    , toRedo = Stack.empty
    }

clearMark : Cell -> Cell
clearMark cell = { cell | state = Unknown }

setHelp : Game -> Game
setHelp game =
    { game
    | grid = Grid.indexedReplaceAll (setCellHelp game.grid) game.grid
    , toUndo = Stack.push game.toUndo (GameRef game)
    , toRedo = Stack.empty
    }

setCellHelp : Grid Cell -> Coordinates -> Cell -> Cell
setCellHelp grid coordinates cell = case cell.hint of
    NoHint -> { cell | help = NoHelp }
    CellsToMark cellsToMark ->
        if isInError grid coordinates cellsToMark then
            { cell | help = Error }
        else
            { cell | help = NoHelp }

isInError : Grid Cell -> Coordinates -> Int -> Bool
isInError grid coordinates cellsToMark =
    Grid.count isUnknown (Grid.neighbours grid coordinates) == 0 && cellsToMark /= numberOfMarkedNeighbours grid coordinates

clearHelp : Game -> Game
clearHelp game =
    { game
    | grid = Grid.replaceAll (\c -> {c|help = NoHelp}) game.grid
    , toUndo = Stack.push game.toUndo (GameRef game)
    , toRedo = Stack.empty
    }

module Game exposing (..)

import Grid exposing (Coordinates, Grid)
import Stack exposing (Stack)

type alias Game =
    { grid: Grid Cell
    , toUndo: Stack GameRef
    , toRedo: Stack GameRef
    }
type GameRef = GameRef Game

type Hint = None | CellsToMark Int
type State = Marked | Unmarked | Unknown
type alias Cell =
    { state: State
    , hint: Hint
    }
type alias GridSize = { width: Int, height: Int }

fromSize: GridSize -> Game
fromSize {width, height} =
    { grid = Grid.from(List.repeat height (List.repeat width { state = Unknown, hint = None }))
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

toggleCell: Cell -> Cell
toggleCell cell = case cell.state of
    Unknown -> markCell cell
    Marked -> unmarkCell cell
    Unmarked -> clearCell cell

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

clearCell  : Cell -> Cell
clearCell cell = { cell | state = Unknown }


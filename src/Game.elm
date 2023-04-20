module Game exposing (..)

import Grid exposing (Coordinates, Grid)
import Stack exposing (Stack)


type alias Game =
    { grid : Grid Cell
    , toUndo : Stack GameRef
    , toRedo : Stack GameRef
    , toggleMode : ToggleMode
    }


type GameRef
    = GameRef Game


type Hint
    = NoHint
    | CellsToMark Int Bool


type State
    = Marked
    | Unmarked
    | Unknown


type Help
    = NoHelp
    | Error


type alias Cell =
    { state : State
    , hint : Hint
    , help : Help
    }


type alias GridSize =
    { width : Int, height : Int }


type ToggleMode
    = Toggling
    | Marking
    | Unmarking
    | Filling


fromSize : GridSize -> Game
fromSize { width, height } =
    { grid = Grid.from (List.repeat height (List.repeat width { state = Unknown, hint = NoHint, help = NoHelp }))
    , toUndo = Stack.empty
    , toRedo = Stack.empty
    , toggleMode = Toggling
    }


undo : Game -> Game
undo game =
    case Stack.pop game.toUndo of
        Just (GameRef previous) ->
            { previous | toRedo = Stack.push game.toRedo (GameRef game) }

        Nothing ->
            game


redo : Game -> Game
redo game =
    case Stack.pop game.toRedo of
        Just (GameRef next) ->
            next

        Nothing ->
            game


toggle : Game -> Coordinates -> Game
toggle game coordinates =
    let
        markCell : Cell -> Cell
        markCell cell =
            { cell | state = Marked }

        unmarkCell : Cell -> Cell
        unmarkCell cell =
            { cell | state = Unmarked }

        clearCell : Cell -> Cell
        clearCell cell =
            { cell | state = Unknown }

        toggleCell : Cell -> Cell
        toggleCell cell =
            case cell.state of
                Unknown ->
                    markCell cell

                Marked ->
                    unmarkCell cell

                Unmarked ->
                    clearCell cell

        clearGridHelp : Grid Cell -> Grid Cell
        clearGridHelp grid =
            Grid.replaceAll (\c -> { c | help = NoHelp }) grid
    in
    case game.toggleMode of
        Toggling ->
            updateGrid game (Grid.update toggleCell game.grid coordinates |> clearGridHelp)

        Marking ->
            updateGrid game (Grid.update markCell game.grid coordinates |> clearGridHelp)

        Unmarking ->
            updateGrid game (Grid.update unmarkCell game.grid coordinates |> clearGridHelp)

        Filling ->
            updateGrid game (fill game.grid coordinates |> clearGridHelp)


updateHint : Game -> Coordinates -> String -> Game
updateHint game coordinates hint =
    let
        updateCellHint : String -> Cell -> Cell
        updateCellHint h cell =
            { cell
                | hint =
                    String.toInt h
                        |> Maybe.map (\value -> (CellsToMark value True))
                        |> Maybe.withDefault NoHint
            }
    in
    updateGrid game (Grid.update (updateCellHint hint) game.grid coordinates)


isOver : Game -> Bool
isOver game =
    let
        isHintUnverified : Grid Cell -> Coordinates -> Cell -> Bool
        isHintUnverified grid coordinates cell =
            case cell.hint of
                NoHint ->
                    False

                CellsToMark cellsToMark _->
                    numberOfMarkedNeighbours grid coordinates /= cellsToMark
    in
    case Grid.count isUnknown game.grid of
        0 ->
            Grid.indexedCount (isHintUnverified game.grid) game.grid == 0

        _ ->
            False


isUnknown : Cell -> Bool
isUnknown cell =
    case cell.state of
        Unknown ->
            True

        _ ->
            False


numberOfMarkedNeighbours : Grid Cell -> Coordinates -> Int
numberOfMarkedNeighbours grid coordinates =
    let
        isCellMarked : Cell -> Bool
        isCellMarked cell =
            case cell.state of
                Marked ->
                    True

                _ ->
                    False
    in
    Grid.neighbours grid coordinates
        |> Grid.count isCellMarked


numberOfUnmarkedNeighbours : Grid Cell -> Coordinates -> Int
numberOfUnmarkedNeighbours grid coordinates =
    let
        isCellUnmarked : Cell -> Bool
        isCellUnmarked cell =
            case cell.state of
                Unmarked ->
                    True

                _ ->
                    False
    in
    Grid.neighbours grid coordinates
        |> Grid.count isCellUnmarked


numberOfUnknownNeighbours : Grid Cell -> Coordinates -> Int
numberOfUnknownNeighbours grid coordinates =
    let
        isCellUnknown : Cell -> Bool
        isCellUnknown cell =
            case cell.state of
                Unknown ->
                    True

                _ ->
                    False
    in
    Grid.neighbours grid coordinates
        |> Grid.count isCellUnknown


numberOfNeighbours : Grid Cell -> Coordinates -> Int
numberOfNeighbours grid coordinates =
    Grid.neighbours grid coordinates |> Grid.count (\_ -> True)


updateGrid : Game -> Grid Cell -> Game
updateGrid game grid =
    { game
        | grid = grid
        , toUndo = Stack.push game.toUndo (GameRef game)
        , toRedo = Stack.empty
    }


clearMarks : Game -> Game
clearMarks game =
    let
        clearMark : Cell -> Cell
        clearMark cell =
            { cell | state = Unknown, help = NoHelp }
    in
    { game
        | grid = Grid.replaceAll clearMark game.grid
        , toUndo = Stack.push game.toUndo (GameRef game)
        , toRedo = Stack.empty
    }


fill : Grid Cell -> Coordinates -> Grid Cell
fill grid coordinates =
    let
        cellHint : Grid Cell -> Coordinates -> Hint
        cellHint g c =
            Grid.get g c
                |> Maybe.map (\cell -> cell.hint)
                |> Maybe.withDefault NoHint

        markUnknown : Cell -> Cell
        markUnknown cell =
            case cell.state of
                Unknown ->
                    { cell | state = Marked }

                _ ->
                    cell

        markKnown : Cell -> Cell
        markKnown cell =
            case cell.state of
                Unknown ->
                    { cell | state = Unmarked }

                _ ->
                    cell
    in
    case cellHint grid coordinates of
        NoHint ->
            grid

        CellsToMark cellsToMark _ ->
            if cellsToMark == numberOfUnknownNeighbours grid coordinates + numberOfMarkedNeighbours grid coordinates then
                Grid.replaceNeighbours markUnknown grid coordinates

            else if cellsToMark == numberOfNeighbours grid coordinates - numberOfUnmarkedNeighbours grid coordinates - numberOfUnknownNeighbours grid coordinates then
                Grid.replaceNeighbours markKnown grid coordinates

            else
                grid


setHelp : Game -> Game
setHelp game =
    let
        isInError : Grid Cell -> Coordinates -> Int -> Bool
        isInError grid coordinates cellsToMark =
            cellsToMark
                < numberOfMarkedNeighbours grid coordinates
                || cellsToMark
                > numberOfNeighbours grid coordinates
                - numberOfUnmarkedNeighbours grid coordinates

        setCellHelp : Grid Cell -> Coordinates -> Cell -> Cell
        setCellHelp grid coordinates cell =
            case cell.hint of
                NoHint ->
                    { cell | help = NoHelp }

                CellsToMark cellsToMark _ ->
                    if isInError grid coordinates cellsToMark then
                        { cell | help = Error }

                    else
                        { cell | help = NoHelp }
    in
    { game
        | grid = Grid.indexedReplaceAll (setCellHelp game.grid) game.grid
        , toUndo = Stack.push game.toUndo (GameRef game)
        , toRedo = Stack.empty
    }


changeToggleMode : Game -> ToggleMode -> Game
changeToggleMode game toggleMode =
    { game | toggleMode = toggleMode }

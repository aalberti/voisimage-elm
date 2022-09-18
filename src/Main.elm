module Main exposing (..)

import Browser
import Grid exposing (Coordinates, Grid, Row, indexedCells)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Stack exposing (Stack)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
    { grid: Grid Cell
    , message: String
    , past: Stack ModelReference
    , future: Stack ModelReference
    }
type ModelReference = ModelReference Model

type Hint = None | CellsToMark Int
type State = Marked | Unmarked | Unknown
type alias Cell =
    { state: State
    , hint: Hint
    }

init : Model
init =
    { grid = Grid.from
        [ [ { state = Unmarked, hint = CellsToMark 0 }
          , { state = Unknown, hint = None }
          , { state = Marked, hint = CellsToMark 2 }
          ]
        , [ { state = Unmarked, hint = CellsToMark 0 }
          , { state = Unknown, hint = None }
          , { state = Marked, hint = CellsToMark 2 }
          ]
        , [ { state = Unmarked, hint = CellsToMark 0 }
          , { state = Unknown, hint = None }
          , { state = Marked, hint = CellsToMark 2 }
          ]
        ]
    , message = ""
    , past = Stack.empty
    , future = Stack.empty
    }

-- UPDATE

type Msg = Mark Coordinates | Unmark Coordinates | Clear Coordinates | Undo | Redo

update : Msg -> Model -> Model
update msg model = case msg of
    Mark coordinates -> updateGrid model (markCell model.grid coordinates)
    Unmark coordinates -> updateGrid model (unmarkCell model.grid coordinates)
    Clear coordinates -> updateGrid model (clearCell model.grid coordinates)
    Undo -> case Stack.pop model.past of
        Just (ModelReference previous) -> { previous | future = Stack.push model.future (ModelReference model) }
        Nothing -> model
    Redo -> case Stack.pop model.future of
        Just (ModelReference next) -> next
        Nothing -> model

updateGrid : Model -> Grid Cell -> Model
updateGrid model grid =
    { model
    | grid = grid
    , past = Stack.push model.past (ModelReference model)
    , future = Stack.empty
    }

markCell : Grid Cell -> Coordinates -> Grid Cell
markCell grid coordinates = Grid.update (\cell -> { cell | state = Marked }) grid coordinates

unmarkCell  : Grid Cell -> Coordinates -> Grid Cell
unmarkCell grid coordinates = Grid.update (\cell -> { cell | state = Unmarked }) grid coordinates

clearCell  : Grid Cell -> Coordinates -> Grid Cell
clearCell grid coordinates = Grid.update (\cell -> { cell | state = Unknown }) grid coordinates

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ button [onClick Undo] [text "undo"]
        , button [onClick Redo] [text "redo"]
        , table
            [ style "border-collapse" "collapse"
            , style "border" "1px solid black"
            , style "-webkit-touch-callout" "none"
            , style "-webkit-user-select" "none"
            , style "-khtml-user-select" "none"
            , style "-moz-user-select" "none"
            , style "-ms-user-select" "none"
            , style "ser-select" "none"
            ]
            [ tbody [] (Grid.indexedRows renderRow model.grid)
            ]
        , text model.message
        ]

renderRow : Int -> Row Cell -> Html Msg
renderRow y row = tr [] (indexedCells (renderCell y) row)

renderCell : Int -> Int -> Cell -> Html Msg
renderCell y x cell  = let additionalStyles = styles cell
    in
        td (
            [ onClick (clickAction cell {x = x, y = y})
            , style "border" "1px solid black"
            , style "height" "20px"
            , style "width" "20px"
            , style "text-align" "center"
            , style "padding" "0"
            ]
        ++ additionalStyles
        )
        [ text (cellToString cell) ]

cellToString: Cell -> String
cellToString cell = case cell.hint of
    None -> ""
    CellsToMark number -> String.fromInt number

styles: Cell -> List (Attribute msg)
styles cell = case cell.state of
    Marked -> [ style "background-color" "black"
              , style "color" "white"
              ]
    Unmarked -> [ style "background-color" "white" ]
    Unknown -> [ style "background-color" "lightgrey" ]

clickAction : Cell -> Coordinates -> Msg
clickAction cell coordinates = case cell.state of
    Unknown -> Mark coordinates
    Marked -> Unmark coordinates
    Unmarked -> Clear coordinates
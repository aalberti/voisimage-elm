module Main exposing (..)

import Browser
import Game exposing (Cell, Game, Hint(..), State(..))
import Grid exposing (Coordinates, Grid, Row, indexedCells)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = Game

init : Game
init = Game.init

-- UPDATE

type Msg = Toggle Coordinates | Undo | Redo

update : Msg -> Model -> Model
update msg model = case msg of
    Toggle coordinates -> Game.toggle model coordinates
    Undo -> Game.undo model
    Redo -> Game.redo model

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
        ]

renderRow : Int -> Row Cell -> Html Msg
renderRow y row = tr [] (indexedCells (renderCell y) row)

renderCell : Int -> Int -> Cell -> Html Msg
renderCell y x cell  = let additionalStyles = styles cell
    in
        td (
            [ onClick (clickAction {x = x, y = y})
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

clickAction : Coordinates -> Msg
clickAction coordinates = Toggle coordinates
module Main exposing (..)

import Browser
import Game exposing (Cell, Game, Hint(..), State(..))
import Grid exposing (Coordinates, Grid, Row, indexedCells)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MAIN

main = Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type Model = Initialization { width: Maybe Int, height: Maybe Int } | Editing Game | Playing Game

init : Model
init = Initialization { width = Nothing, height = Nothing }

-- UPDATE

type Msg = WidthUpdated String | HeightUpdated String | Initialize
    | Undo | Redo
    | HintUpdated Coordinates String | Play
    | Toggle Coordinates

update : Msg -> Model -> Model
update msg model = case (model, msg) of
    (Initialization size, WidthUpdated width) -> Initialization { size | width = String.toInt width }
    (Initialization size, HeightUpdated height) -> Initialization { size | height = String.toInt height }
    (Initialization size, Initialize) -> Editing (Game.fromSize {width = orZero size.width, height = orZero size.height})
    (Editing game, HintUpdated coordinates hint) -> Editing (Game.updateHint game coordinates hint)
    (Editing game, Undo) -> Editing (Game.undo game)
    (Editing game, Redo) -> Editing (Game.redo game)
    (Editing game, Play) -> Playing (game)
    (Playing game, Toggle coordinates) -> Playing (Game.toggle game coordinates)
    (Playing game, Undo) -> Playing (Game.undo game)
    (Playing game, Redo) -> Playing (Game.redo game)
    _ -> model

orZero : Maybe Int -> Int
orZero maybeInt = Maybe.withDefault 0 maybeInt

-- VIEW

view : Model -> Html Msg
view model = case model of
    Initialization { width, height } -> div []
        [ text "Width"
        , input [type_ "number", placeholder "width", value (sizeToString width), onInput WidthUpdated ][]
        , text "Height"
        , input [type_ "number", placeholder "height", value (sizeToString height), onInput HeightUpdated ][]
        , button [ onClick Initialize ] [text "Initialize"]
        ]
    Editing game ->  div []
        [ gridView hintEditor game
        , button [ onClick Play ] [ text "play"]
        ]
    Playing game -> gridView cellView game

sizeToString : (Maybe Int) -> String
sizeToString size =
    Maybe.map String.fromInt size |> Maybe.withDefault ""

gridView: (Int -> Int -> Cell -> Html Msg) -> Game -> Html Msg
gridView cellRenderer game = div []
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
        [ tbody [] (Grid.indexedRows (renderRow cellRenderer) game.grid)
        ]
    ]

renderRow : (Int -> Int -> Cell -> Html Msg) -> Int -> Row Cell -> Html Msg
renderRow cellRenderer y row = tr [] (indexedCells (cellRenderer y) row)

cellView : Int -> Int -> Cell -> Html Msg
cellView y x cell  =
    td (
        [ onClick (Toggle {x = x, y = y})
        , style "border" "1px solid black"
        , style "height" "20px"
        , style "width" "20px"
        , style "text-align" "center"
        , style "padding" "0"
        ]
        ++ (styles cell)
    )
    [ text (cellToString cell) ]

hintEditor : Int -> Int -> Cell -> Html Msg
hintEditor y x cell  =
    td (
        [ style "border" "1px solid black"
        , style "height" "20px"
        , style "width" "20px"
        , style "text-align" "center"
        , style "padding" "0"
        ]
    )
    [ input
        (
            [ value (cellToString cell)
            , onInput (HintUpdated {x = x, y = y})
            , style "height" "20px"
            , style "width" "20px"
            , style "border" "none"
            ]
            ++ (styles cell)
        )
        []
    ]

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
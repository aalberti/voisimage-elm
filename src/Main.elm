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

type Model = Initialization { width: Maybe Int, height: Maybe Int } | Playing Game

init : Model
init = Initialization { width = Nothing, height = Nothing }

-- UPDATE

type Msg = Toggle Coordinates | Undo | Redo | WidthUpdated String | HeightUpdated String | Initialize

update : Msg -> Model -> Model
update msg model = case (model, msg) of
    (Playing game, Toggle coordinates) -> Playing (Game.toggle game coordinates)
    (Playing game, Undo) -> Playing (Game.undo game)
    (Playing game, Redo) -> Playing (Game.redo game)
    (Initialization size, WidthUpdated width) -> Initialization { size | width = String.toInt width }
    (Initialization size, HeightUpdated height) -> Initialization { size | height = String.toInt height }
    (Initialization size, Initialize) -> Playing (Game.fromSize {width = orZero size.width, height = orZero size.height})
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
    Playing game ->
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
                [ tbody [] (Grid.indexedRows renderRow game.grid)
                ]
            ]

sizeToString : (Maybe Int) -> String
sizeToString size =
    Maybe.map String.fromInt size |> Maybe.withDefault ""

renderRow : Int -> Row Cell -> Html Msg
renderRow y row = tr [] (indexedCells (renderCell y) row)

renderCell : Int -> Int -> Cell -> Html Msg
renderCell y x cell  = let additionalStyles = styles cell
    in
        td (
            [ onClick (Toggle {x = x, y = y})
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
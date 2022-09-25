port module Main exposing (..)

import Browser
import Codec exposing (decode, encode)
import Game exposing (Cell, Game, Hint(..), State(..))
import Grid exposing (Coordinates, Grid, Row, indexedCellsMap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Error)
import Json.Encode exposing (Value)
import Stack

-- MAIN

main = Browser.element
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }

-- MODEL

type Model = Initialization { width: Maybe Int, height: Maybe Int, message: String } | Editing Game | Playing Game

init : Value -> (Model, Cmd Msg)
init flags =
    ( case (decode flags) of
        Ok g -> Editing {grid = g, toUndo = Stack.empty, toRedo = Stack.empty}
        Err error -> (Initialization { width = Nothing, height = Nothing, message = Decode.errorToString error })
    , Cmd.none
    )

-- UPDATE

type Msg = WidthUpdated String | HeightUpdated String | Initialize
    | Undo | Redo
    | HintUpdated Coordinates String | Play
    | Toggle Coordinates

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (model, msg) of
    (Initialization size, WidthUpdated width) -> (Initialization { size | width = String.toInt width }, Cmd.none)
    (Initialization size, HeightUpdated height) -> (Initialization { size | height = String.toInt height }, Cmd.none)
    (Initialization size, Initialize) -> (Editing (Game.fromSize {width = orZero size.width, height = orZero size.height}), Cmd.none)
    (Editing game, HintUpdated coordinates hint) -> (Editing (Game.updateHint game coordinates hint), Cmd.none)
    (Editing game, Undo) -> (Editing (Game.undo game), Cmd.none)
    (Editing game, Redo) -> (Editing (Game.redo game), Cmd.none)
    (Editing game, Play) -> (Playing (game), Cmd.none)
    (Playing game, Toggle coordinates) -> (Playing (Game.toggle game coordinates), Cmd.none)
    (Playing game, Undo) -> (Playing (Game.undo game), Cmd.none)
    (Playing game, Redo) -> (Playing (Game.redo game), Cmd.none)
    _ -> (model, Cmd.none)

orZero : Maybe Int -> Int
orZero maybeInt = Maybe.withDefault 0 maybeInt

-- PORTS

port setStorage : Value -> Cmd msg
port resetStorage : () -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) = update msg oldModel
    in case newModel of
        Initialization _ ->
            ( newModel
            , Cmd.batch [ resetStorage (), cmds ]
            )
        Editing game ->
            ( newModel
            , Cmd.batch [ encode game.grid |> setStorage, cmds ]
            )
        Playing game ->
            ( newModel
            , Cmd.batch [ encode game.grid |> setStorage, cmds ]
            )

-- VIEW

view : Model -> Html Msg
view model = case model of
    Initialization { width, height, message } -> div []
        [ text "Width"
        , input [type_ "number", placeholder "width", value (sizeToString width), onInput WidthUpdated ][]
        , text "Height"
        , input [type_ "number", placeholder "height", value (sizeToString height), onInput HeightUpdated ][]
        , button [ onClick Initialize ] [text "Initialize"]
        , text message
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
        [ tbody [] (Grid.indexedRowsMap (renderRow cellRenderer) game.grid)
        ]
    ]

renderRow : (Int -> Int -> Cell -> Html Msg) -> Int -> Row Cell -> Html Msg
renderRow cellRenderer y row = tr [] (indexedCellsMap (cellRenderer y) row)

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
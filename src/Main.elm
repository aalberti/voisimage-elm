port module Main exposing (..)

import Browser
import Codec exposing (decode, decodeFromString, encode)
import Game exposing (Cell, Game, Hint(..), State(..), ToggleMode(..))
import Grid exposing (Coordinates, Grid, Row, indexedCellsMap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Error)
import Json.Encode exposing (Value)
import Stack



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias InitializationParameters =
    { width : Maybe Int
    , height : Maybe Int
    , json : String
    , message : String
    }


type Model
    = Initialization InitializationParameters
    | Editing Game
    | Playing Game
    | GameOver Game


init : Value -> ( Model, Cmd Msg )
init flags =
    ( case decode flags of
        Ok g ->
            Editing { grid = g, toggleMode = Toggling, toUndo = Stack.empty, toRedo = Stack.empty }

        Err error ->
            Initialization { width = Nothing, height = Nothing, json = "", message = Decode.errorToString error }
    , Cmd.none
    )



-- UPDATE


type Msg
    = WidthUpdated String
    | HeightUpdated String
    | JsonUpdated String
    | Initialize
    | Undo
    | Redo
    | HintUpdated Coordinates String
    | Play
    | Toggle Coordinates
    | NewGame
    | ClearMarks
    | ShowHelp
    | ToggleModeChangedTo ToggleMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initialization params, WidthUpdated width ) ->
            ( Initialization { params | width = String.toInt width }, Cmd.none )

        ( Initialization params, HeightUpdated height ) ->
            ( Initialization { params | height = String.toInt height }, Cmd.none )

        ( Initialization params, JsonUpdated json ) ->
            ( Initialization { params | json = json }, Cmd.none )

        ( Initialization params, Initialize ) ->
            initializeGame params

        ( Editing game, HintUpdated coordinates hint ) ->
            ( Editing (Game.updateHint game coordinates hint), Cmd.none )

        ( Editing game, Undo ) ->
            ( Editing (Game.undo game), Cmd.none )

        ( Editing game, Redo ) ->
            ( Editing (Game.redo game), Cmd.none )

        ( Editing game, Play ) ->
            ( Playing game, Cmd.none )

        ( Playing game, Toggle coordinates ) ->
            gameOverOr (Game.toggle game coordinates)

        ( Playing game, Undo ) ->
            ( Playing (Game.undo game), Cmd.none )

        ( Playing game, Redo ) ->
            ( Playing (Game.redo game), Cmd.none )

        ( Playing game, ClearMarks ) ->
            ( Playing (Game.clearMarks game), Cmd.none )

        ( Playing game, ShowHelp ) ->
            ( Playing (Game.setHelp game), Cmd.none )

        ( Playing game, ToggleModeChangedTo toggleMode ) ->
            ( Playing (Game.changeToggleMode game toggleMode), Cmd.none )

        ( _, NewGame ) ->
            ( Initialization { width = Nothing, height = Nothing, json = "", message = "" }, Cmd.none )

        _ ->
            ( model, Cmd.none )


initializeGame : InitializationParameters -> ( Model, Cmd Msg )
initializeGame params =
    case params.json of
        "" ->
            ( Editing (Game.fromSize { width = orZero params.width, height = orZero params.height }), Cmd.none )

        _ ->
            case decodeFromString params.json of
                Ok g ->
                    ( Editing { grid = g, toggleMode = Toggling, toUndo = Stack.empty, toRedo = Stack.empty }, Cmd.none )

                Err error ->
                    ( Initialization { params | message = Decode.errorToString error }, Cmd.none )


gameOverOr : Game -> ( Model, Cmd msg )
gameOverOr game =
    if Game.isOver game then
        ( GameOver game, Cmd.none )

    else
        ( Playing game, Cmd.none )


orZero : Maybe Int -> Int
orZero maybeInt =
    Maybe.withDefault 0 maybeInt



-- PORTS


port setStorage : Value -> Cmd msg


port resetStorage : () -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    case newModel of
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

        GameOver _ ->
            ( newModel
            , Cmd.batch [ resetStorage (), cmds ]
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Initialization { width, height, json, message } ->
            div []
                [ div []
                    [ text "Width"
                    , input [ type_ "number", placeholder "width", value (sizeToString width), onInput WidthUpdated ] []
                    , text "Height"
                    , input [ type_ "number", placeholder "height", value (sizeToString height), onInput HeightUpdated ] []
                    ]
                , div []
                    [ text "or"
                    , textarea [ cols 40, rows 10, placeholder "json here", value json, onInput JsonUpdated ] []
                    ]
                , div []
                    [ button [ onClick Initialize ] [ text "Initialize" ]
                    , text message
                    ]
                ]

        Editing game ->
            div []
                [ button [ onClick Undo ] [ text "undo" ]
                , button [ onClick Redo ] [ text "redo" ]
                , gridView hintEditor game
                , button [ onClick Play ] [ text "play" ]
                , button [ onClick NewGame ] [ text "new game" ]
                ]

        Playing game ->
            playingScreen game

        GameOver game ->
            div []
                [ text "Congratulations"
                , gridView cellViewer game
                , button [ onClick NewGame ] [ text "moar" ]
                ]


playingScreen : Game -> Html Msg
playingScreen game =
    let
        radio : String -> (String -> Msg) -> Bool -> List (Html Msg)
        radio label command isChecked =
            [ input [ type_ "radio", onInput command, checked isChecked ] [ text label ]
            , text label
            ]

        isToggling : Game -> Bool
        isToggling g =
            case g.toggleMode of
                Toggling ->
                    True

                _ ->
                    False

        isMarking : Game -> Bool
        isMarking g =
            case g.toggleMode of
                Marking ->
                    True

                _ ->
                    False

        isUnmarking : Game -> Bool
        isUnmarking g =
            case g.toggleMode of
                Unmarking ->
                    True

                _ ->
                    False

        isFilling : Game -> Bool
        isFilling g =
            case g.toggleMode of
                Filling ->
                    True

                _ ->
                    False
    in
    div []
        [ div []
            [ button [ onClick Undo ] [ text "undo" ]
            , button [ onClick Redo ] [ text "redo" ]
            ]
        , div []
            (radio "toggle" (\_ -> ToggleModeChangedTo Toggling) (isToggling game)
                ++ radio "mark" (\_ -> ToggleModeChangedTo Marking) (isMarking game)
                ++ radio "unmark" (\_ -> ToggleModeChangedTo Unmarking) (isUnmarking game)
                ++ radio "fill" (\_ -> ToggleModeChangedTo Filling) (isFilling game)
            )
        , div []
            [ gridView cellMarker game
            ]
        , div []
            [ button [ onClick NewGame ] [ text "new game" ]
            , button [ onClick ClearMarks ] [ text "clear" ]
            , button [ onClick ShowHelp ] [ text "help" ]
            ]
        ]


sizeToString : Maybe Int -> String
sizeToString size =
    Maybe.map String.fromInt size |> Maybe.withDefault ""


gridView : (Int -> Int -> Cell -> Html Msg) -> Game -> Html Msg
gridView cellRenderer game =
    div []
        [ table
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
renderRow cellRenderer y row =
    tr [] (indexedCellsMap (cellRenderer y) row)


cellMarker : Int -> Int -> Cell -> Html Msg
cellMarker y x cell =
    td
        ([ onClick (Toggle { x = x, y = y })
         , style "border" "1px solid black"
         , style "height" "20px"
         , style "width" "20px"
         , style "text-align" "center"
         , style "padding" "0"
         ]
            ++ styles cell
        )
        [ text (cellToString cell) ]


cellViewer : Int -> Int -> Cell -> Html Msg
cellViewer _ _ cell =
    td
        ([ style "height" "10px"
         , style "width" "10px"
         , style "padding" "0"
         ]
            ++ styles cell
        )
        []


hintEditor : Int -> Int -> Cell -> Html Msg
hintEditor y x cell =
    td
        [ style "border" "1px solid black"
        , style "height" "20px"
        , style "width" "20px"
        , style "text-align" "center"
        , style "padding" "0"
        ]
        [ input
            ([ value (cellToString cell)
             , onInput (HintUpdated { x = x, y = y })
             , style "height" "20px"
             , style "width" "20px"
             , style "border" "none"
             ]
                ++ styles cell
            )
            []
        ]


cellToString : Cell -> String
cellToString cell =
    case cell.hint of
        NoHint ->
            ""

        CellsToMark number ->
            String.fromInt number


styles : Cell -> List (Attribute msg)
styles cell =
    (case cell.state of
        Marked ->
            [ style "background-color" "black"
            , style "color" "white"
            ]

        Unmarked ->
            [ style "background-color" "white" ]

        Unknown ->
            [ style "background-color" "lightgrey" ]
    )
        ++ (case cell.help of
                Game.Error ->
                    [ style "border" "solid 2px red" ]

                _ ->
                    []
           )

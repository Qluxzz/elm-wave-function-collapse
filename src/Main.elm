module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html
import Html.Attributes exposing (id)
import Html.Events
import Json.Decode as Decode
import String exposing (fromInt)



{-
   Sudoku solver based on collapsing waveform algorithm

   Rules of sudoku:
    * 9x9 grid (sub grids of 3*3) where a number 1-9 can be placed
    * There can't be any duplicate numbers in any axis or in the same sub grid
-}


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onKeyUp keyDecoder
                    ]
        }


type alias Model =
    { grid : Dict ( Int, Int ) (Maybe Int)
    , focusedCell : Maybe ( Int, Int )
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = Dict.fromList grid, focusedCell = Nothing }, Cmd.none )


type CellState
    = Okay
    | Error
    | Undefined


subGrid : ( Int, Int ) -> ( Int, Int )
subGrid ( x, y ) =
    ( floor (toFloat x / 3), floor (toFloat y / 3) )


validate : Dict ( Int, Int ) (Maybe Int) -> Dict ( Int, Int ) CellState
validate g =
    let
        filledCells : List ( Int, Int )
        filledCells =
            Dict.foldr
                (\pos ->
                    \v ->
                        \acc ->
                            case v of
                                Just _ ->
                                    pos :: acc

                                Nothing ->
                                    acc
                )
                []
                g
    in
    Dict.empty


grid : List ( ( Int, Int ), Maybe Int )
grid =
    List.foldl
        (\x ->
            \acc ->
                acc ++ (List.range 0 8 |> List.map (\y -> Tuple.pair (Tuple.pair x y) Nothing))
        )
        []
        (List.range 0 8)


type Msg
    = FocusCell ( Int, Int )
    | EnterNumber Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusCell c ->
            ( { model | focusedCell = Just c }, Cmd.none )

        EnterNumber n ->
            case model.focusedCell of
                Nothing ->
                    ( model, Cmd.none )

                Just c ->
                    ( { model | grid = Dict.update c (\_ -> Just (Just n)) model.grid }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body =
        [ Html.div []
            (Dict.toList model.grid
                |> List.map
                    (\( ( x, y ), v ) ->
                        Html.div [ id (String.fromInt x ++ "," ++ String.fromInt y), Html.Attributes.classList [ ( "focused", model.focusedCell == Just ( x, y ) ) ], Html.Events.onClick (FocusCell ( x, y )) ] [ Html.text (v |> Maybe.map fromInt |> Maybe.withDefault "") ]
                    )
            )
        ]
    }


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


between : comparable -> comparable -> comparable -> Bool
between min max value =
    value >= min && value <= max


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            let
                normalizedChar =
                    Char.toUpper char

                code =
                    Char.toCode normalizedChar

                zero =
                    48

                nine =
                    57

                space =
                    32
            in
            if between zero nine code then
                EnterNumber (code - zero)

            else
                NoOp

        _ ->
            NoOp

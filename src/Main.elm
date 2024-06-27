module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html
import Html.Attributes exposing (id)
import Html.Events
import Json.Decode as Decode
import Set
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
    ( { grid = Dict.fromList generateGrid, focusedCell = Nothing }, Cmd.none )


type CellState
    = Okay
    | Error
    | Undefined


subGrid : ( Int, Int ) -> ( Int, Int )
subGrid ( y, x ) =
    ( floor (toFloat y / 3), floor (toFloat x / 3) )


horizontal : Dict ( Int, Int ) (Maybe Int) -> ( Int, Int ) -> List Int
horizontal g ( y, x ) =
    List.range 0 8
        |> List.filter (\x_ -> x_ /= x)
        |> List.filterMap (\x_ -> Dict.get ( y, x_ ) g)
        |> List.filterMap identity


vertical : Dict ( Int, Int ) (Maybe Int) -> ( Int, Int ) -> List Int
vertical g ( y, x ) =
    List.range 0 8
        |> List.filter (\y_ -> y_ /= y)
        |> List.filterMap (\y_ -> Dict.get ( y_, x ) g)
        |> List.filterMap identity


valuesInSubGrid : Dict ( Int, Int ) (Maybe Int) -> ( Int, Int ) -> List Int
valuesInSubGrid g pos =
    let
        ( y, x ) =
            subGrid pos

        offsets =
            [ ( 0, 0 )
            , ( 0, 1 )
            , ( 0, 2 )
            , ( 1, 0 )
            , ( 1, 1 )
            , ( 1, 2 )
            , ( 2, 0 )
            , ( 2, 1 )
            , ( 2, 2 )
            ]
    in
    offsets
        |> List.map (Tuple.mapBoth ((+) (y * 3)) ((+) (x * 3)))
        |> List.filter (\p -> p /= pos)
        |> List.map (\p -> Dict.get p g)
        |> List.filterMap identity
        |> List.filterMap identity


type alias ValidationResult =
    { state : CellState
    , possibleValues : Set.Set Int
    }


validNumbersForCell : Set.Set number
validNumbersForCell =
    Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


validate : Dict ( Int, Int ) (Maybe Int) -> Dict ( Int, Int ) ValidationResult
validate g =
    g
        |> Dict.map
            (\pos ->
                \v_ ->
                    let
                        used =
                            [ horizontal, vertical, valuesInSubGrid ]
                                |> List.map (\x_ -> x_ g pos)
                                |> List.concat
                                |> Set.fromList

                        p =
                            Set.diff validNumbersForCell used
                    in
                    case v_ of
                        Nothing ->
                            { state = Undefined, possibleValues = p }

                        Just v__ ->
                            -- Validate axis and subgrid
                            if Set.member v__ used then
                                { state = Error, possibleValues = p }

                            else
                                { state = Okay, possibleValues = p }
            )


generateGrid : List ( ( Int, Int ), Maybe Int )
generateGrid =
    List.foldl
        (\y ->
            \acc ->
                acc ++ (List.range 0 8 |> List.map (\x -> Tuple.pair (Tuple.pair x y) Nothing))
        )
        []
        (List.range 0 8)


type Msg
    = FocusCell ( Int, Int )
    | FocusNextCell
    | FocusPreviousCell
    | FocusCellAbove
    | FocusCellBelow
    | EnterNumber Int
    | EnterNumberForCell ( Int, Int ) Int
    | ClearFocusedCell
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusCell c ->
            ( { model | focusedCell = Just c }, Cmd.none )

        FocusNextCell ->
            let
                next =
                    model.focusedCell
                        |> Maybe.map (\( y, x ) -> ( y, modBy 9 (x + 1) ))
                        |> Maybe.withDefault ( 0, 0 )
                        |> Just
            in
            ( { model | focusedCell = next }, Cmd.none )

        FocusPreviousCell ->
            let
                previous =
                    model.focusedCell
                        |> Maybe.map (\( y, x ) -> ( y, modBy 9 (x - 1) ))
                        |> Maybe.withDefault ( 8, 0 )
                        |> Just
            in
            ( { model | focusedCell = previous }, Cmd.none )

        FocusCellAbove ->
            let
                above =
                    model.focusedCell
                        |> Maybe.map (\( y, x ) -> ( modBy 9 (y - 1), x ))
                        |> Maybe.withDefault ( 0, 0 )
                        |> Just
            in
            ( { model | focusedCell = above }, Cmd.none )

        FocusCellBelow ->
            let
                below =
                    model.focusedCell
                        |> Maybe.map (\( y, x ) -> ( modBy 9 (y + 1), x ))
                        |> Maybe.withDefault ( 0, 0 )
                        |> Just
            in
            ( { model | focusedCell = below }, Cmd.none )

        ClearFocusedCell ->
            case model.focusedCell of
                Just c ->
                    ( { model | grid = Dict.insert c Nothing model.grid }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EnterNumber n ->
            case model.focusedCell of
                Nothing ->
                    ( model, Cmd.none )

                Just c ->
                    ( { model | grid = Dict.insert c (Just n) model.grid }, Cmd.none )

        EnterNumberForCell pos n ->
            ( { model | grid = Dict.insert pos (Just n) model.grid }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        validation =
            validate model.grid
    in
    { title = "Document Title"
    , body =
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.div [ Html.Attributes.class "grid" ]
                (Dict.toList model.grid
                    |> List.map
                        (\( ( y, x ) as pos, v ) ->
                            let
                                v_ =
                                    Dict.get pos validation

                                isFocused =
                                    model.focusedCell == Just pos
                            in
                            Html.div
                                [ id (String.fromInt y ++ "," ++ String.fromInt x)
                                , Html.Attributes.classList
                                    [ ( "focused", isFocused )
                                    , ( "error", Maybe.map .state v_ == Just Error )
                                    , ( "okay", Maybe.map .state v_ == Just Okay )
                                    ]
                                , Html.Events.onClick (FocusCell pos)
                                ]
                                (case v of
                                    Just v__ ->
                                        [ Html.text (fromInt v__) ]

                                    Nothing ->
                                        List.map (\s -> Html.div [ Html.Attributes.class "possible", Html.Events.onClick (EnterNumberForCell pos s) ] [ Html.text (fromInt s) ]) (Maybe.map (.possibleValues >> Set.toList) v_ |> Maybe.withDefault [])
                                )
                        )
                )
            , Html.div
                [ Html.Attributes.class "sub-grid" ]
                (List.range 0 8 |> List.map (\_ -> Html.div [] []))
            ]
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
            case keyValue of
                "Backspace" ->
                    ClearFocusedCell

                "ArrowLeft" ->
                    FocusPreviousCell

                "ArrowRight" ->
                    FocusNextCell

                "ArrowUp" ->
                    FocusCellAbove

                "ArrowDown" ->
                    FocusCellBelow

                _ ->
                    NoOp

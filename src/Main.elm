module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html
import Html.Attributes exposing (id)
import Html.Events
import Json.Decode as Decode
import Platform.Cmd as Cmd
import Random
import Random.Extra
import Set
import String exposing (fromInt)
import Time



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
            \model ->
                Sub.batch
                    (Browser.Events.onKeyUp keyDecoder
                        :: (if not model.solved then
                                [ Time.every 10 (\_ -> SolveSingleStep) ]

                            else
                                []
                           )
                    )
        }


type alias Model =
    { grid : Dict ( Int, Int ) (Maybe Int)
    , focusedCell : Maybe ( Int, Int )
    , solverSpeedMs : Int
    , solved : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { grid = Dict.fromList generateGrid, focusedCell = Nothing, solverSpeedMs = 1000, solved = True }, Cmd.none )


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
    | RandomCell ( Int, Int )
    | AutoSolve
    | SolveSingleStep
    | Reset
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
            if Set.member n validNumbersForCell then
                ( { model | grid = Dict.insert pos (Just n) model.grid }, Cmd.none )

            else
                ( model, Cmd.none )

        AutoSolve ->
            ( { model | solved = False }, Cmd.none )

        SolveSingleStep ->
            -- Find cell with lowest "entropy", lowest amount of possible values, continue until all cells have been filled
            let
                validation =
                    validate model.grid

                cellsWithEntropy : List ( ( Int, Int ), Int )
                cellsWithEntropy =
                    Dict.foldr
                        (\pos ->
                            \{ possibleValues } ->
                                \acc ->
                                    case Dict.get pos model.grid |> Maybe.andThen identity of
                                        Just _ ->
                                            acc

                                        Nothing ->
                                            ( pos, Set.size possibleValues ) :: acc
                        )
                        []
                        validation

                cellsWithLowestEntropy : List ( ( Int, Int ), Int )
                cellsWithLowestEntropy =
                    case cellsWithEntropy |> List.sortBy Tuple.second of
                        head :: rest ->
                            let
                                minAmount =
                                    Tuple.second head
                            in
                            head :: List.filter (\x -> Tuple.second x == minAmount) rest

                        x ->
                            x

                -- Take a random cell with the lowest entropy
                -- Select a random possible value for that cell
                -- Continue until all cells is filled
            in
            case cellsWithLowestEntropy of
                head :: _ ->
                    if Tuple.second head == 0 then
                        let
                            ( m, msg_ ) =
                                init ()
                        in
                        ( { m | solved = False }, msg_ )

                    else
                        ( model, Random.generate RandomCell (Random.Extra.sample (cellsWithLowestEntropy |> List.map Tuple.first) |> Random.map (Maybe.withDefault ( 0, 0 ))) )

                _ ->
                    ( { model | solved = True }, Cmd.none )

        RandomCell pos ->
            case Dict.get pos model.grid |> Maybe.andThen identity of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    let
                        x =
                            validate model.grid |> Dict.get pos |> Maybe.map .possibleValues
                    in
                    case x of
                        Nothing ->
                            Debug.todo "Shouldn't be possible"

                        Just v ->
                            ( model, Random.generate (EnterNumberForCell pos) (Random.Extra.sample (Set.toList v) |> Random.map (Maybe.withDefault 0)) )

        Reset ->
            init ()

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        validation =
            validate model.grid
    in
    { title = "Sudoku wave function collapse algorithm"
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
        , Html.div [ Html.Attributes.class "actions" ]
            [ Html.button [ Html.Events.onClick AutoSolve ] [ Html.text "Auto solve" ]
            , Html.button [ Html.Events.onClick SolveSingleStep ] [ Html.text "Step" ]
            , Html.button [ Html.Events.onClick Reset ] [ Html.text "Reset" ]
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

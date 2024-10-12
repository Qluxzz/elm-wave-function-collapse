module Main exposing (generateGrid, getPossibleValuesForCells, main)

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
import Random.Set
import Set
import String exposing (fromInt)
import Time



{-
   Sudoku solver based on wave collapse algorithm

   Rules of sudoku:
    * 9x9 grid (sub grids of 3*3) where a number 1-9 can be placed
    * There can't be any duplicate numbers in any axis or in the same sub grid
-}


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init Nothing
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    (Browser.Events.onKeyUp keyDecoder
                        :: (if not model.solved then
                                [ Time.every 50 (\_ -> SolveSingleStep) ]

                            else
                                []
                           )
                    )
        }


type alias Grid =
    Dict ( Int, Int ) (Maybe Int)


type alias Model =
    { grid : Grid
    , gridTemplate : Maybe Grid
    , focusedCell : Maybe ( Int, Int )
    , solved : Bool
    }


init : Maybe Grid -> ( Model, Cmd Msg )
init startingGrid =
    let
        g =
            startingGrid |> Maybe.withDefault (Dict.fromList generateGrid)
    in
    ( { grid = g
      , gridTemplate = startingGrid
      , focusedCell = Nothing
      , solved = True
      }
    , Cmd.none
    )


type Msg
    = FocusCell ( Int, Int )
    | FocusNextCell
    | FocusPreviousCell
    | FocusCellAbove
    | FocusCellBelow
    | EnterNumber Int
    | EnterNumberForCell ( Int, Int ) Int
    | ClearFocusedCell
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
            ( { model | focusedCell = focusCellWithDefault (\( y, x ) -> ( y, modBy 9 (x + 1) )) model.focusedCell }, Cmd.none )

        FocusPreviousCell ->
            ( { model | focusedCell = focusCellWithDefault (\( y, x ) -> ( y, modBy 9 (x - 1) )) model.focusedCell }, Cmd.none )

        FocusCellAbove ->
            ( { model | focusedCell = focusCellWithDefault (\( y, x ) -> ( modBy 9 (y - 1), x )) model.focusedCell }, Cmd.none )

        FocusCellBelow ->
            ( { model | focusedCell = focusCellWithDefault (\( y, x ) -> ( modBy 9 (y + 1), x )) model.focusedCell }, Cmd.none )

        ClearFocusedCell ->
            case model.focusedCell of
                Just c ->
                    let
                        updatedGrid =
                            Dict.insert c Nothing model.grid
                    in
                    ( { model | grid = updatedGrid }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EnterNumber n ->
            case model.focusedCell of
                Just focusedCell ->
                    if Set.member n (getPossibleValuesForCell model.grid focusedCell) then
                        ( { model | grid = Dict.insert focusedCell (Just n) model.grid }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EnterNumberForCell pos n ->
            if Set.member n (getPossibleValuesForCell model.grid pos) then
                ( { model | grid = Dict.insert pos (Just n) model.grid }, Cmd.none )

            else
                ( model, Cmd.none )

        AutoSolve ->
            ( { model | solved = False, gridTemplate = Just model.grid }, Cmd.none )

        SolveSingleStep ->
            let
                filledCells =
                    Dict.filter (\_ -> \v -> v /= Nothing) model.grid

                ( positions, fewestPossibleValues ) =
                    Dict.foldr
                        (\pos ->
                            \possibleValues ->
                                \(( positions_, lowest ) as acc) ->
                                    if Dict.member pos filledCells then
                                        acc

                                    else
                                        let
                                            amount =
                                                Set.size possibleValues
                                        in
                                        case Basics.compare amount lowest of
                                            GT ->
                                                -- The amount is higher than the current lowest
                                                -- Ignore position and continue with next position
                                                acc

                                            EQ ->
                                                -- The amount is equal to that of the current lowest
                                                -- Add position to list of possible positions
                                                ( pos :: positions_, lowest )

                                            LT ->
                                                -- The amount is lower than current lowest
                                                -- Replace current list of positions
                                                ( [ pos ], amount )
                        )
                        ( [], 10 )
                        (getPossibleValuesForCells model.grid)
            in
            -- We're stuck, we have a cell which have no possible values
            -- Just reset and try to solve again
            -- Solving the contradiction is usually trickier (Straight from the WFC wizard himself https://x.com/OskSta/status/1218477384095141889)
            if fewestPossibleValues == 0 then
                let
                    ( m, msg_ ) =
                        init model.gridTemplate
                in
                ( { m | solved = False }, msg_ )

            else
                case positions of
                    [] ->
                        ( { model | solved = True }, Cmd.none )

                    _ ->
                        ( model
                        , Random.generate identity (generatePossibleValueForPosition positions model.grid)
                        )

        Reset ->
            if Just model.grid == model.gridTemplate then
                init Nothing

            else
                init model.gridTemplate

        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Sudoku wave function collapse algorithm"
    , body =
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.div [ Html.Attributes.class "grid" ]
                (Dict.toList model.grid
                    |> List.map
                        (\( ( y, x ) as pos, v ) ->
                            Html.div
                                [ id (String.fromInt y ++ "," ++ String.fromInt x)
                                , Html.Attributes.classList
                                    [ ( "focused", model.focusedCell == Just pos )
                                    ]
                                , Html.Events.onClick (FocusCell pos)
                                ]
                                (case v of
                                    Just v__ ->
                                        [ Html.text (fromInt v__) ]

                                    Nothing ->
                                        List.map
                                            (\s ->
                                                Html.div
                                                    [ Html.Attributes.class "possible"
                                                    , Html.Events.onClick (EnterNumberForCell pos s)
                                                    ]
                                                    [ Html.text (fromInt s) ]
                                            )
                                            (getPossibleValuesForCell model.grid pos |> Set.toList)
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
            , Html.button [ Html.Events.onClick Reset ]
                [ Html.text
                    (if model.gridTemplate == Just model.grid then
                        "Reset All"

                     else
                        "Reset"
                    )
                ]
            ]
        , Html.div [ Html.Attributes.class "attributions" ]
            [ Html.span [] [ Html.text "Inspired by: ", Html.a [ Html.Attributes.href "https://www.youtube.com/watch?v=2SuvO4Gi7uY", Html.Attributes.target "_blank", Html.Attributes.rel "noopener" ] [ Html.text "Superpositions, Sudoku, the Wave Function Collapse algorithm" ] ]
            ]
        ]
    }



-- HELPERS


horizontal : Grid -> ( Int, Int ) -> List Int
horizontal g ( y, x ) =
    List.range 0 8
        |> List.filter (\x_ -> x_ /= x)
        |> List.filterMap (\x_ -> Dict.get ( y, x_ ) g |> Maybe.andThen identity)


vertical : Grid -> ( Int, Int ) -> List Int
vertical g ( y, x ) =
    List.range 0 8
        |> List.filter (\y_ -> y_ /= y)
        |> List.filterMap (\y_ -> Dict.get ( y_, x ) g |> Maybe.andThen identity)


valuesInSubGrid : Grid -> ( Int, Int ) -> List Int
valuesInSubGrid g (( y, x ) as pos) =
    let
        ( subGridY, subGridX ) =
            ( floor (toFloat y / 3), floor (toFloat x / 3) )

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
        |> List.map (Tuple.mapBoth ((+) (subGridY * 3)) ((+) (subGridX * 3)))
        |> List.filter (\p -> p /= pos)
        |> List.filterMap (\p -> Dict.get p g |> Maybe.andThen identity)


validNumbersForCell : Set.Set number
validNumbersForCell =
    Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


getPossibleValuesForCells : Grid -> Dict ( Int, Int ) (Set.Set Int)
getPossibleValuesForCells grid =
    Dict.map
        (\pos -> \_ -> getPossibleValuesForCell grid pos)
        grid


getPossibleValuesForCell : Grid -> ( Int, Int ) -> Set.Set Int
getPossibleValuesForCell grid pos =
    let
        rules =
            [ horizontal, vertical, valuesInSubGrid ]
    in
    rules
        |> List.concatMap (\rule -> rule grid pos)
        |> Set.fromList
        |> Set.diff validNumbersForCell


generateGrid : List ( ( Int, Int ), Maybe Int )
generateGrid =
    -- 9*9 = 81, range is end inclusive
    List.range 0 80 |> List.map (\x -> ( ( modBy 9 x, x // 9 ), Nothing ))


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


generatePossibleValueForPosition : List ( Int, Int ) -> Grid -> Random.Generator Msg
generatePossibleValueForPosition positions grid =
    Random.Extra.sample positions
        |> Random.andThen
            (\maybePos ->
                case maybePos of
                    Just pos ->
                        getPossibleValuesForCell grid pos
                            |> Random.Set.sample
                            |> Random.map
                                (\maybeVal ->
                                    case maybeVal of
                                        Just v ->
                                            EnterNumberForCell pos v

                                        Nothing ->
                                            NoOp
                                )

                    Nothing ->
                        Random.constant NoOp
            )


center : ( Int, Int )
center =
    ( 4, 4 )


focusCellWithDefault : (( Int, Int ) -> ( Int, Int )) -> Maybe ( Int, Int ) -> Maybe ( Int, Int )
focusCellWithDefault transform input =
    input |> Maybe.map transform |> Maybe.withDefault center |> Just

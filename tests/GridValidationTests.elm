module GridValidationTests exposing (..)

import Dict exposing (Dict)
import Expect
import Main exposing (CellState(..), generateGrid, validate)
import Set
import Test exposing (Test, describe, test)


cellStateTestCases : List { input : Dict ( Int, Int ) (Maybe Int), expected : Dict ( Int, Int ) CellState }
cellStateTestCases =
    [ { input = Dict.fromList [], expected = Dict.fromList [] }
    , { input = Dict.fromList [ ( ( 0, 0 ), Just 2 ) ]
      , expected = Dict.fromList [ ( ( 0, 0 ), Okay ) ]
      }
    , { input = Dict.fromList [ ( ( 3, 7 ), Just 2 ), ( ( 7, 5 ), Just 2 ) ]
      , expected = Dict.fromList [ ( ( 3, 7 ), Okay ), ( ( 7, 5 ), Okay ) ]
      }
    ]


suite : Test
suite =
    describe "Validate sudoku rules"
        [ describe "Cell states"
            (List.map
                (\{ input, expected } ->
                    test (Debug.toString input ++ " = " ++ Debug.toString expected) <|
                        \_ ->
                            validate input |> Dict.map (\_ -> \v -> v.state) |> Expect.equalDicts expected
                )
                cellStateTestCases
            )
        , describe "Possible values"
            [ test "Should be possible to select five" <|
                \_ ->
                    let
                        g =
                            generateGrid |> Dict.fromList |> Dict.insert ( 0, 1 ) (Just 5)

                        validation =
                            validate g
                    in
                    Maybe.map2
                        (\a -> \b -> Expect.equalSets a b)
                        (Dict.get ( 4, 4 ) validation |> Maybe.map .possibleValues)
                        (Just (Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]))
                        |> Maybe.withDefault (Expect.fail "hello")
            , test "Should not be possible to select one" <|
                \_ ->
                    let
                        g =
                            generateGrid |> Dict.fromList |> Dict.insert ( 0, 0 ) (Just 1)

                        validation =
                            validate g
                    in
                    Maybe.map2
                        (\a -> \b -> Expect.equalSets a b)
                        (Dict.get ( 1, 1 ) validation |> Maybe.map .possibleValues)
                        (Just (Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ]))
                        |> Maybe.withDefault (Expect.fail "hello")
            ]
        ]

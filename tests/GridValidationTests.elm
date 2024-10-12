module GridValidationTests exposing (suite)

import Dict
import Expect
import Main exposing (defaultGrid, getPossibleValuesForCells)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Validate sudoku rules"
        [ describe "Possible values"
            [ test "Should be possible to select five" <|
                \_ ->
                    let
                        g =
                            defaultGrid |> Dict.insert ( 0, 1 ) (Just 5)

                        validation =
                            getPossibleValuesForCells g
                    in
                    Maybe.map2
                        (\a -> \b -> Expect.equalSets a b)
                        (Dict.get ( 4, 4 ) validation)
                        (Just (Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]))
                        |> Maybe.withDefault (Expect.fail "hello")
            , test "Should not be possible to select one" <|
                \_ ->
                    let
                        g =
                            defaultGrid |> Dict.insert ( 0, 0 ) (Just 1)

                        validation =
                            getPossibleValuesForCells g
                    in
                    Maybe.map2
                        (\a -> \b -> Expect.equalSets a b)
                        (Dict.get ( 1, 1 ) validation)
                        (Just (Set.fromList [ 2, 3, 4, 5, 6, 7, 8, 9 ]))
                        |> Maybe.withDefault (Expect.fail "hello")
            ]
        ]

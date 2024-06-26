module GridValidationTests exposing (..)

import Dict exposing (Dict)
import Expect
import Main exposing (CellState(..), validate)
import Test exposing (Test, describe, test)


type alias TestCase =
    { input : Dict ( Int, Int ) (Maybe Int)
    , expected : Dict ( Int, Int ) CellState
    }


testCases : List { input : Dict ( Int, Int ) (Maybe Int), expected : Dict ( Int, Int ) CellState }
testCases =
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
        (List.map
            (\{ input, expected } ->
                test (Debug.toString input ++ " = " ++ Debug.toString expected) <|
                    \_ ->
                        validate input |> Expect.equalDicts expected
            )
            testCases
        )

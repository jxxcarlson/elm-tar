module Tests
    exposing
        ( stringTestData
        , binaryTestData
        , checkListAsString
        , checkListAsBinary
        )

import Test.Table as T
import TestTools as Tools
import Expect
import Test exposing (test)
import Bytes exposing (Bytes)
import Tar
import Hex


{-|

> checkListAsString stringTestData

     [(0,True),(1,True),(2,True)]

-}
checkListAsString : List TestDatum -> List ( Int, Bool )
checkListAsString testList =
    evalListAsString testList
        |> List.indexedMap (\i ( a, b ) -> ( i, List.map Just a == b ))


{-|

> checkListAsBinary binaryTestData
> [(0,True),(1,True)] : List ( Int, Bool )
-}
checkListAsBinary : List TestDatum -> List ( Int, Bool )
checkListAsBinary testList =
    testList |> List.indexedMap (\i datum -> ( i, checkBinary datum ))


stringTestData =
    [ [ "", "", "a", "ab" ]
    , [ "abc", "abcd", "abcde", "abcdef" ]
    , [ "abcdefg", "abcdefgh" ]
    ]


binaryTestData =
    [ [ "", "", "00", "65" ]
    , [ "A1B2656667" ]
    ]


binaryTestDatum =
    [ "", "", "00", "65" ]


type alias TestDatum =
    List String


type alias TestResult =
    ( List String, List (Maybe String) )


evalAsString : TestDatum -> TestResult
evalAsString datum =
    ( datum, Tools.extractStringDataFromArchive <| Tools.makeTextFileArchive datum )


evalListAsString : List TestDatum -> List TestResult
evalListAsString testList =
    List.map evalAsString testList


evalAsBinary : TestDatum -> ( TestDatum, TestDatum )
evalAsBinary datum =
    ( datum
    , datum
        |> Tools.makeBinaryFileArchive
        |> Tar.extractArchive
        |> List.map Tuple.second
        |> List.map Tools.stringValue
    )


checkBinary : TestDatum -> Bool
checkBinary datum =
    datum
        |> evalAsBinary
        |> (\( a, b ) -> a == b)

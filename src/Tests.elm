module Tests exposing (testData, evalListAsString, checkListAsString)

import Test.Table as T
import TestTools as Tools
import Expect
import Test exposing (test)
import Bytes exposing (Bytes)
import Tar


-- stringTest =
--     test "foo"
--         (T.testTable "string data"
--             [ ( a1, d1 ) ]
--             (\( a, d ) -> extractStringDataFromArchive a |> Expect.equal d)
--         )


testData =
    [ [ "", "1a", "ab" ]
    , [ "abc", "abcd", "abcde", "abcdef" ]
    , [ "abcdef", "abcdefg" ]
    , [ "abcdefgh" ]
    ]


type alias StringTestDatum =
    List String


type alias StringTestResult =
    ( List String, List (Maybe String) )


evalAsString : StringTestDatum -> StringTestResult
evalAsString datum =
    ( datum, Tools.extractStringDataFromArchive <| Tools.makeTextFileArchive datum )


evalListAsString : List StringTestDatum -> List StringTestResult
evalListAsString testList =
    List.map evalAsString testList


checkListAsString : List StringTestDatum -> List ( Int, Bool )
checkListAsString testList =
    evalListAsString testList
        |> List.indexedMap (\i ( a, b ) -> ( i, List.map Just a == b ))


a1 =
    Tools.makeTextFileArchive [ "", "" ]


d1 =
    [ Just "", Just "" ]

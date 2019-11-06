module TestTar exposing (..)

import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SHA256
import String.Graphemes
import Tar
import Test exposing (..)


tearsOfJoy : Char
tearsOfJoy =
    '😂'


defaultMetaData =
    Tar.defaultMetadata


suite : Test
suite =
    describe "tar tests"
        [ describe "utf-8 characters" <|
            let
                utf8Test name value =
                    test name <|
                        \_ ->
                            let
                                data =
                                    Tar.StringData value

                                input =
                                    [ ( defaultMetaData, data ) ]

                                result =
                                    input
                                        |> Tar.createArchive
                                        |> Tar.extractArchive
                            in
                            case result of
                                [ ( _, resultData ) ] ->
                                    resultData |> Expect.equal data

                                _ ->
                                    Expect.fail "invalid"
            in
            [ utf8Test "tears of joy" (String.fromChar tearsOfJoy)
            , utf8Test "emoji" "絵文字"
            ]
        , describe "string normalization"
            [ fuzz (Fuzz.tuple ( Fuzz.intRange 0 300, Fuzz.string )) "string is no longer than expected" <|
                \( size, string ) ->
                    normalizeString size string
                        |> Encode.getStringWidth
                        |> Expect.equal size
            , fuzz (Fuzz.tuple ( Fuzz.intRange 1 300, Fuzz.string )) "string ends with null" <|
                \( size, string ) ->
                    normalizeString size string
                        |> String.endsWith "\u{0000}"
                        |> Expect.equal True
            , test "LANDNÁMABÓK" <|
                \_ ->
                    normalizeString 6 "LANDNÁMABÓK"
                        |> Encode.getStringWidth
                        |> Expect.equal 6
            , test "complex utf case" <|
                \_ ->
                    "ab\u{1F9B8}\u{1F3FD}cd"
                        |> normalizeString 9
                        |> String.toList
                        |> Expect.equal (List.map Char.fromCode [ 0x61, 0x62, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ])
            ]
        , describe "hashes" <|
            let
                hashTest name items expected =
                    test name <|
                        \_ ->
                            Tar.createArchive items
                                |> SHA256.fromBytes
                                |> SHA256.toHex
                                |> Expect.equal expected
            in
            [ hashTest "empty" [] "5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
            , hashTest "string" [ ( Tar.defaultMetadata, Tar.StringData "foo" ) ] "f85654c84b6e9ca6989fc42fd5814063bbdb27ce116e2baad34f210f42a6145d"
            , hashTest "bytes" [ ( Tar.defaultMetadata, Tar.BinaryData (Encode.encode (Encode.string "foo")) ) ] "f85654c84b6e9ca6989fc42fd5814063bbdb27ce116e2baad34f210f42a6145d"
            , hashTest "file name too long 1" [ ( { defaultMetaData | filename = String.repeat 101 "a" }, Tar.StringData "foo" ) ] "e1fd20591cce478c384df041219d12d2fe8634e2aef1bfd42c0bdd15a532da83"
            , hashTest "file name too long 2" [ ( { defaultMetaData | filename = String.repeat 102 "a" }, Tar.StringData "foo" ) ] "e1fd20591cce478c384df041219d12d2fe8634e2aef1bfd42c0bdd15a532da83"
            ]
        ]


{-| Encode a string of a specific length.

  - the string capped a `length - 1`
  - the string is padded with the null character to the desired length

-}
normalizeString : Int -> String -> String
normalizeString desiredLength str =
    case desiredLength of
        0 ->
            -- just to be safe. otherwise unbounded recursion
            ""

        _ ->
            let
                dropped =
                    str
                        -- first `desiredLength` characters
                        -- but this function should produce
                        -- `desiredLength` bytes, not characters
                        |> String.left desiredLength
                        -- so this step is required
                        |> dropRightLoop (desiredLength - 1)

                paddingSize =
                    desiredLength - Encode.getStringWidth dropped
            in
            dropped ++ String.repeat paddingSize "\u{0000}"


dropRightLoop : Int -> String -> String
dropRightLoop desiredLength str =
    if Encode.getStringWidth str > desiredLength then
        dropRightLoop desiredLength (String.Graphemes.dropRight 1 str)

    else
        str

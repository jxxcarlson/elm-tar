module TestTar exposing (..)

import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
                        |> dropLeftLoop (desiredLength - 1)

                paddingSize =
                    desiredLength - Encode.getStringWidth dropped
            in
            dropped ++ String.repeat paddingSize "\u{0000}"


dropLeftLoop : Int -> String -> String
dropLeftLoop desiredLength str =
    if Encode.getStringWidth str > desiredLength then
        dropLeftLoop desiredLength (String.dropRight 1 str)

    else
        str

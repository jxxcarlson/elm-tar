module TestTar exposing (..)

import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hex.Convert
import Octal
import SHA256
import String.Graphemes
import Tar
import Test exposing (..)


tearsOfJoy : Char
tearsOfJoy =
    '😂'


defaultMetaData =
    Tar.defaultMetaData


process input =
    case input of
        [ ( meta, Tar.BinaryData data ) ] ->
            case Decode.decode (Decode.string meta.fileSize) data of
                Just v ->
                    [ ( meta, Tar.StringData v ) ]

                _ ->
                    Debug.todo "not a just"

        [ ( meta, Tar.StringData data ) ] ->
            input

        _ ->
            Debug.todo ("invalid list: " ++ Debug.toString input)


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

                                meta =
                                    { defaultMetaData | fileSize = Encode.getStringWidth value }

                                input =
                                    [ ( meta, data ) ]

                                {-
                                   _ =
                                       input
                                           |> Tar.createArchive
                                           |> (\b -> Decode.decode (Decode.string 800) b)
                                           |> Maybe.withDefault ""
                                           |> String.replace "\u{0000}" "."
                                           |> Hex.Convert.blocks 16
                                           |> Debug.log ""
                                -}
                                result =
                                    input
                                        |> Tar.createArchive
                                        |> Tar.extractArchive
                            in
                            case result of
                                ( newMeta, Tar.StringData newValue ) :: _ ->
                                    ( newMeta, newValue )
                                        |> Expect.equal ( meta, value )

                                _ ->
                                    Expect.fail ("invalid: " ++ Debug.toString result)
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

        {-
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
               , hashTest "string" [ ( Tar.defaultMetaData, Tar.StringData "foo" ) ] "f85654c84b6e9ca6989fc42fd5814063bbdb27ce116e2baad34f210f42a6145d"
               , hashTest "bytes" [ ( Tar.defaultMetaData, Tar.BinaryData (Encode.encode (Encode.string "foo")) ) ] "f85654c84b6e9ca6989fc42fd5814063bbdb27ce116e2baad34f210f42a6145d"
               , hashTest "file name too long 1" [ ( { defaultMetaData | filename = String.repeat 101 "a" }, Tar.StringData "foo" ) ] "e1fd20591cce478c384df041219d12d2fe8634e2aef1bfd42c0bdd15a532da83"
               , hashTest "file name too long 2" [ ( { defaultMetaData | filename = String.repeat 102 "a" }, Tar.StringData "foo" ) ] "e1fd20591cce478c384df041219d12d2fe8634e2aef1bfd42c0bdd15a532da83"
               , hashTest "file name too long 3" [ ( { defaultMetaData | filename = String.repeat 102 "a" }, Tar.BinaryData (Encode.encode (Encode.string "foo")) ) ] "e1fd20591cce478c384df041219d12d2fe8634e2aef1bfd42c0bdd15a532da83"
               ]
        -}
        , describe "octal" <|
            let
                octalListTest int list =
                    test ("octal " ++ String.fromInt int) <|
                        \_ ->
                            int
                                |> Octal.digits
                                |> List.reverse
                                |> Expect.equal list

                octalHexTest width int string =
                    test ("octal hex " ++ String.fromInt width ++ " " ++ String.fromInt int) <|
                        \_ ->
                            Octal.encode width int
                                |> Encode.encode
                                |> Hex.Convert.toString
                                |> Expect.equal string

                octalRoundtrip value =
                    test ("octal roundtrip " ++ String.fromInt value) <|
                        \_ ->
                            Octal.encode 8 value
                                |> Encode.encode
                                |> Decode.decode (Octal.decode 8)
                                |> Expect.equal (Just value)
            in
            [ octalListTest 2001 [ 1, 2, 7, 3 ]
            , octalListTest 2019 [ 3, 4, 7, 3 ]
            , octalListTest 8 [ 0, 1 ]
            , octalListTest 64 [ 0, 0, 1 ]

            -- hex
            -- roundtrip
            , fuzz (Fuzz.intRange 0 2097151 {- 0o0777_7777 -}) "fuzz roundtrip" <|
                \value ->
                    Octal.encode 8 value
                        |> Encode.encode
                        |> Decode.decode (Octal.decode 8)
                        |> Expect.equal (Just value)
            , test "encode oct of 10001111" <|
                \_ ->
                    Octal.encode 8 143
                        |> Encode.encode
                        |> Decode.decode (Decode.string 8)
                        |> Expect.equal (Just "0000217\u{0000}")
            , test "decode oct of 10001111" <|
                \_ ->
                    "00002170"
                        |> Encode.string
                        |> Encode.encode
                        |> Decode.decode (Octal.decode 8)
                        |> Expect.equal (Just 143)
            ]
        , describe "decode foo"
            [ test "decode foo bytes" <|
                \_ ->
                    case Tar.extractArchive fooTar of
                        ( meta, data ) :: _ ->
                            meta.mode.group
                                |> Expect.equal { write = False, read = True, execute = False }

                        _ ->
                            Expect.fail "problem"
            , test "roundtrip " <|
                \_ ->
                    let
                        once =
                            Tar.extractArchive fooTar
                                |> process

                        twice =
                            once
                                |> Tar.createArchive
                                |> Tar.extractArchive
                                |> process
                    in
                    twice
                        |> Expect.equal once
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


fooTar =
    let
        bytes =
            [ 0x66
            , 0x6F
            , 0x6F
            , 0x2E
            , 0x74
            , 0x78
            , 0x74
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x36
            , 0x34
            , 0x34
            , 0x00
            , 0x30
            , 0x30
            , 0x30
            , 0x31
            , 0x37
            , 0x35
            , 0x30
            , 0x00
            , 0x30
            , 0x30
            , 0x30
            , 0x31
            , 0x37
            , 0x35
            , 0x30
            , 0x00
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x30
            , 0x34
            , 0x00
            , 0x31
            , 0x33
            , 0x35
            , 0x36
            , 0x30
            , 0x36
            , 0x33
            , 0x31
            , 0x32
            , 0x35
            , 0x37
            , 0x00
            , 0x30
            , 0x31
            , 0x33
            , 0x35
            , 0x30
            , 0x30
            , 0x00
            , 0x20
            , 0x30
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x75
            , 0x73
            , 0x74
            , 0x61
            , 0x72
            , 0x20
            , 0x20
            , 0x00
            , 0x66
            , 0x6F
            , 0x6C
            , 0x6B
            , 0x65
            , 0x72
            , 0x74
            , 0x64
            , 0x65
            , 0x76
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x66
            , 0x6F
            , 0x6C
            , 0x6B
            , 0x65
            , 0x72
            , 0x74
            , 0x64
            , 0x65
            , 0x76
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            , 0x00
            ]

        padding =
            List.repeat (10240 - List.length bytes) (Encode.unsignedInt8 0)
    in
    Encode.sequence (List.map Encode.unsignedInt8 bytes ++ padding)
        |> Encode.encode

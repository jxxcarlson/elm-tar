module Octal exposing (binaryDigits, decode, digits, encode, integerValueofOctalList)

{-| Octal numbers

Tar is weird in that it uses ascii-encoded octal (base 8) numbers.

the decimal number 48 is the position of the '0' character in the ascii table.

-}

import Bitwise
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)


{-| Per the spec

> All other fields are zero-filled octal numbers in ASCII. Each numeric field of width w contains w minus 1 digits, and a null.

-}
encode : Int -> Int -> Encoder
encode width n =
    let
        octalDigits =
            digits n
                |> List.take (n - 1)
                |> List.map (\x -> Encode.unsignedInt8 (x + 48))

        padding =
            List.repeat (width - List.length octalDigits - 1) (Encode.unsignedInt8 48)
    in
    Encode.sequence (padding ++ octalDigits ++ [ Encode.unsignedInt8 0 ])


decode : Int -> Decoder Int
decode n =
    -- Note: octal numbers are null-terminated. so we must decode an extra byte
    Decode.map2 (\k _ -> k)
        (Decode.loop { remaining = n - 1, accum = 0 } decodeHelp)
        Decode.unsignedInt8


decodeHelp { remaining, accum } =
    if remaining >= 4 then
        Decode.map
            (\word1 ->
                let
                    byte1 =
                        Bitwise.shiftRightZfBy 24 word1 |> Bitwise.and 0xFF

                    byte2 =
                        Bitwise.shiftRightZfBy 16 word1 |> Bitwise.and 0xFF

                    byte3 =
                        Bitwise.shiftRightZfBy 8 word1 |> Bitwise.and 0xFF

                    byte4 =
                        Bitwise.and 0xFF word1
                in
                Decode.Loop
                    { remaining = remaining - 4
                    , accum =
                        accum
                            |> (*) 8
                            |> (+) (byte1 - 48)
                            |> (*) 8
                            |> (+) (byte2 - 48)
                            |> (*) 8
                            |> (+) (byte3 - 48)
                            |> (*) 8
                            |> (+) (byte4 - 48)
                    }
            )
            (Decode.unsignedInt32 Bytes.BE)

    else if remaining > 0 then
        Decode.map (\new -> Decode.Loop { remaining = remaining - 1, accum = 8 * accum + (new - 48) }) Decode.unsignedInt8

    else
        Decode.succeed (Decode.Done accum)


{-| octal digits, most significant digit first

    octalList 2001
        --> [ 3, 7, 2, 1 ]

-}
digits : Int -> List Int
digits n =
    octalDigitsHelp n []


octalDigitsHelp : Int -> List Int -> List Int
octalDigitsHelp n accum =
    if n < 8 then
        n :: accum

    else
        let
            lo =
                modBy 8 n

            hi =
                n // 8
        in
        octalDigitsHelp hi (lo :: accum)


integerValueofOctalList : List Int -> Int
integerValueofOctalList octalDigits =
    integerValueOfOctalListAux octalDigits 0


integerValueOfOctalListAux : List Int -> Int -> Int
integerValueOfOctalListAux octalDigits acc =
    case octalDigits of
        [] ->
            acc

        d :: remainder ->
            integerValueOfOctalListAux (List.drop 1 octalDigits) (8 * acc + d)


binaryList : Int -> List Int
binaryList n =
    if n < 2 then
        [ n ]

    else
        let
            lo =
                modBy 2 n

            hi =
                n // 2
        in
        lo :: binaryList hi


binaryDigits : Int -> Int -> List Int
binaryDigits k n =
    binaryList n |> List.reverse |> padList k 0


padList : Int -> a -> List a -> List a
padList n padding list =
    if List.length list >= n then
        list

    else
        padding :: padList (n - 1) padding list

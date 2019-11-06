module Octal exposing (binaryDigits, digits, integerValueofOctalList, octalEncoder)

import Bytes.Encode as Encode exposing (Encoder)


{-| Per the spec

> All other fields are zero-filled octal numbers in ASCII. Each numeric field of width w contains w minus 1 digits, and a null.

-}
octalEncoder : Int -> Int -> Encoder
octalEncoder width n =
    let
        octalDigits =
            digits n
                |> List.take (n - 1)
                |> List.map (\x -> Encode.unsignedInt8 (x + 48))

        padding =
            List.repeat (width - List.length octalDigits) (Encode.unsignedInt8 48)
    in
    Encode.sequence (padding ++ octalDigits)


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

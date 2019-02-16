module Octal exposing (binaryDigits, integerValueofOctalList, octalEncoder, octalList)

import Bytes.Encode as Encode exposing (Encoder, encode)


octalEncoder : Int -> Int -> Encoder
octalEncoder width n =
    octalList n
        |> List.reverse
        |> padList width 0
        |> List.map (\x -> x + 48)
        |> List.map Encode.unsignedInt8
        |> Encode.sequence



-- octalAsciiFromOctalString str =
--     octalList n
--         |> List.reverse
--         |> padList width 0
--         |> List.map (\x -> x + 48)
-- encodeFileMode : String -> Bytes
-- |> List.map Encode.unsignedInt8
-- |> Encode.sequence


{-|

> octalList 2001
> [1,2,7,3]
> Last significant digit first
-}
octalList : Int -> List Int
octalList n =
    if n < 8 then
        [ n ]
    else
        let
            lo =
                modBy 8 n

            hi =
                n // 8
        in
            lo :: octalList hi


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

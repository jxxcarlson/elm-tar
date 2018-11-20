module Octal exposing (octalEncoder)

import Bytes.Encode as Encode exposing (encode)


octalEncoder width n =
    octalList n
        |> List.reverse
        |> padList width 0
        |> List.map (\x -> x + 48)
        |> List.map Encode.unsignedInt8
        |> Encode.sequence


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
            lo :: (octalList hi)


padList : Int -> a -> List a -> List a
padList n padding list =
    if List.length list >= n then
        list
    else
        padding :: (padList (n - 1) padding list)

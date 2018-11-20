module CheckSum exposing (sumEncoder)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..), loop, map, succeed)
import Bytes.Encode as Encode exposing (encode)
import Octal exposing (octalEncoder)


{-|

> Encode.string "Hello" |> encode |> checkSum.sum
> 244 : Int
-}
byteSum : Bytes -> Int
byteSum bytes =
    bytes
        |> intListFromBytes
        |> List.sum
        |> (\x -> x + 16)


sumEncoder : Bytes -> Encode.Encoder
sumEncoder bytes =
    bytes |> byteSum |> octalEncoder 6



--
-- NOT EXPOSED
--


intListFromBytes : Bytes -> List Int
intListFromBytes bytes =
    bytes
        |> Decode.decode (decodeBytes (Bytes.width bytes) Decode.unsignedInt8)
        |> Maybe.withDefault []



-- sum256 : List Int -> Int
-- sum256 intList =
--     modBy 256 (List.sum intList)


decodeBytes : Int -> Decoder a -> Decoder (List a)
decodeBytes len decoder =
    loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)
    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder

module CheckSum exposing (checksum, sumBytes)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Step(..), loop, map, succeed)
import Bytes.Encode as Encode
import Octal


checksum : Bytes -> Encode.Encoder
checksum bytes =
    bytes
        |> sumBytes
        |> Octal.encode 8


{-| Sum all the bytes in a `Bytes`.
-}
sumBytes : Bytes -> Int
sumBytes bytes =
    let
        decoder =
            Decode.loop { remaining = Bytes.width bytes, accum = 16 } sumBytesHelp
    in
    case Decode.decode decoder bytes of
        Just v ->
            v

        Nothing ->
            0


sumBytesHelp { remaining, accum } =
    if remaining > 0 then
        Decode.map (\new -> Decode.Loop { remaining = remaining - 1, accum = new + accum }) Decode.unsignedInt8

    else
        Decode.succeed (Decode.Done accum)

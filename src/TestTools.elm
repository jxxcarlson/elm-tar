module TestTools
    exposing
        ( getBinaryDataAsStringAt
        , getBinaryDataAt
        , getMetaDataAt
        , getStringDataAt
        , makeTextFileArchive
        , makeBinaryFileArchive
        , extractStringDataFromArchive
        , extractBinaryDataFromArchive
        , null
        , stringValue
        )

import Bytes exposing (Bytes)
import Bytes.Encode as E
import Hex
import Utility
import Tar exposing (Data(..), MetaData, createArchive, defaultMetadata, extractArchive)


{- Examples

   > getMetaDataAt 0 extractedData
   > getStringDataAt 0 extractedData

   > getBinaryDataAsStringAt 1 extractedData |> Maybe.map (String.left 20)
   > getMetaDataAt 1 extractedData

-}
--
-- TEST DATA
--


{-| Return a string representation of the data
-}
stringValue : Data -> String
stringValue datum =
    case datum of
        StringData str ->
            str

        BinaryData bytes ->
            Hex.fromBytes bytes


null =
    E.encode (E.unsignedInt8 0)


makeTextFileArchive : List String -> Bytes
makeTextFileArchive contentList =
    createArchive <|
        List.indexedMap (\i c -> ( { defaultMetadata | filename = fileNameOfInt i }, StringData c ))
            contentList


makeBinaryFileArchive : List String -> Bytes
makeBinaryFileArchive contentList =
    createArchive <|
        List.indexedMap (\i c -> ( { defaultMetadata | filename = binaryFileNameOfInt i }, BinaryData c ))
            (List.map Hex.toBytes contentList |> List.map (Maybe.withDefault null))


fileNameOfInt : Int -> String
fileNameOfInt k =
    "f" ++ String.fromInt k ++ ".txt"


binaryFileNameOfInt : Int -> String
binaryFileNameOfInt k =
    "f" ++ String.fromInt k ++ ".bin"


extractStringDataFromArchive : Bytes -> List (Maybe String)
extractStringDataFromArchive bytes =
    let
        data : List ( MetaData, Data )
        data =
            extractArchive bytes

        n =
            List.length data
    in
        List.map (\i -> getStringDataAt i data) (List.range 0 (n - 1))


extractBinaryDataFromArchive : Bytes -> List (Maybe Bytes)
extractBinaryDataFromArchive bytes =
    let
        data : List ( MetaData, Data )
        data =
            extractArchive bytes

        n =
            List.length data
    in
        List.map (\i -> getBinaryDataAt i data) (List.range 0 (n - 1))


{-|

> getBinaryDataAt 1 extractedData |> Maybe.map Hex.fromBytes
> Just "0001A1FF"
-}
getBinaryData : ( MetaData, Data ) -> Maybe Bytes
getBinaryData dataPair =
    case Tuple.second dataPair of
        BinaryData bytes ->
            Just bytes

        StringData str ->
            Nothing


{-|

> getStringDataAt 0 extractedData
> Just ("This is a test.")
-}
getStringData : ( MetaData, Data ) -> Maybe String
getStringData dataPair =
    case Tuple.second dataPair of
        StringData str ->
            Just str

        BinaryData bytes ->
            Nothing


getMetaData : ( MetaData, Data ) -> MetaData
getMetaData stuff =
    stuff
        |> Tuple.first


getDataPairAt : Int -> List ( MetaData, Data ) -> Maybe ( MetaData, Data )
getDataPairAt k outputList =
    Utility.listGetAt k outputList


getMetaDataAt : Int -> List ( MetaData, Data ) -> Maybe MetaData
getMetaDataAt k outputList =
    outputList
        |> getDataPairAt k
        |> Maybe.map getMetaData


getBinaryDataAt : Int -> List ( MetaData, Data ) -> Maybe Bytes
getBinaryDataAt k list =
    list
        |> getDataPairAt k
        |> Maybe.andThen getBinaryData


getBinaryDataAsStringAt : Int -> List ( MetaData, Data ) -> Maybe String
getBinaryDataAsStringAt k list =
    list
        |> getBinaryDataAt k
        |> Maybe.map Hex.fromBytes


getStringDataAt : Int -> List ( MetaData, Data ) -> Maybe String
getStringDataAt k list =
    list
        |> getDataPairAt k
        |> Maybe.andThen getStringData

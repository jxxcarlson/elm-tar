module TestTools
    exposing
        ( archive
        , extractedData
        , getBinaryDataAsStringAt
        , getBinaryDataAt
        , getMetaDataAt
        , getStringDataAt
        , makeTextFileArchive
        , extractStringDataFromArchive
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


makeTextFileArchive : List String -> Bytes
makeTextFileArchive contentList =
    createArchive <|
        List.indexedMap (\i c -> ( { defaultMetadata | filename = fileNameOfInt i }, StringData c ))
            contentList


fileNameOfInt : Int -> String
fileNameOfInt k =
    "f" ++ String.fromInt k ++ ".txt"


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


hexData =
    "0001A1FF"


null =
    E.encode (E.unsignedInt8 0)


someBytes =
    Hex.toBytes hexData |> Maybe.withDefault null


metadata =
    { defaultMetadata | filename = "test.txt" }


text =
    "Thisisat"


text1 =
    "This is a test."


metadata2 =
    { defaultMetadata | filename = "foo.binary" }


archive =
    createArchive [ ( metadata, StringData text ), ( metadata2, BinaryData someBytes ) ]


extractedData =
    extractArchive archive



--
-- TEST FUNCTIONS
--


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

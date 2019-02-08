module Test exposing (archive, extractedData, getBinaryDataAsStringAt, getBinaryDataAt, getMetaDataAt, getStringDataAt)

import Bytes exposing (Bytes)
import Bytes.Encode as E
import Hex
import List.Extra
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


null =
    E.encode (E.unsignedInt8 0)


someBytes =
    Hex.toBytes "0001A1FF" |> Maybe.withDefault null


metadata =
    { defaultMetadata | filename = "test123.txt" }


text =
    "This is a test (ho ho ho).\nIt is a frabjous day!"


metadata2 =
    { defaultMetadata | filename = "foo.binary" }


archive =
    createArchive [ ( metadata, StringData text ), ( metadata2, BinaryData someBytes ) ]


extractedData =
    extractArchive archive



--
-- TEST FUNCTIONS
--


getBinaryData : ( MetaData, Data ) -> Maybe Bytes
getBinaryData dataPair =
    case Tuple.second dataPair of
        BinaryData bytes ->
            Just bytes

        StringData str ->
            Nothing


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
    List.Extra.getAt k outputList


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

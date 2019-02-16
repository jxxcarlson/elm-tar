module Test
    exposing
        ( arx
        , isStringPipelineOK
        , isBinaryPipelineOK
        , archive
        , extractedData
        , getBinaryDataAsStringAt
        , getBinaryDataAt
        , getMetaDataAt
        , getStringDataAt
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


arx =
    createArchive
        [ ( { defaultMetadata | filename = "test1.csv" }, StringData "" )
        , ( { defaultMetadata | filename = "test2.csv" }, StringData "" )
        ]


{-| Crude test:

> import Test
> Test.isStringPipelineOK

-}
isStringPipelineOK =
    getStringDataAt 0 extractedData == Just text


{-| Crude test:

> import Test
> Test.isBinaryPipelineOK

-}
isBinaryPipelineOK =
    (getBinaryDataAt 1 extractedData |> Maybe.map Hex.fromBytes) == Just hexData


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

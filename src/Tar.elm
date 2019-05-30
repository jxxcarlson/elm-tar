module Tar
    exposing
        ( Data(..)
        , MetaData
        , createArchive
        , extractArchive
        , testArchive
        , encodeFiles
        , encodeTextFile
        , encodeTextFiles
        , defaultMetadata
        )

{-| Use

encodeMetaData

       createArchive : List ( MetaData, Data ) -> Bytes

to create4 a tar archive from arbitrary set of files which may contain either text or binary
data. To extract files from an archive, imitate this example:

       extractArchive testArchive

For more details, see the README. See also the demo app `./examples/Main.elm`

@docs Data, MetaData, createArchive, extractArchive, testArchive, encodeFiles, encodeTextFile, encodeTextFiles, defaultMetadata

-}

{-
   XXXX module Tar exposing (Data(..), MetaData, createArchive, extractArchive, testArchive, encodeFiles, encodeTextFile, encodeTextFiles, defaultMetadata)

-}

import Bytes exposing (..)
import Bytes.Decode as Decode exposing (Decoder, Step(..), decode)
import Bytes.Encode as Encode exposing (encode)
import Char
import Utility
import CheckSum
import Octal exposing (octalEncoder)
import Time exposing (Posix)
import Hex


--
-- TYPES
--


{-| Use `StringData String` for text data,
`BinaryData Bytes` for binary data, e.g.,
`StringData "This is a test"` or
`BinaryData someBytes`
-}
type Data
    = StringData String
    | BinaryData Bytes


{-| A MetaData value contains the information, e.g.,
file name and file length, needed to construct the header
for a file in the tar archive. You may use `defaultMetadata` as
a starting point, modifying only what is needed.
-}
type alias MetaData =
    { filename : String
    , mode : Mode
    , ownerID : Int
    , groupID : Int
    , fileSize : Int
    , lastModificationTime : Int
    , linkIndicator : Link
    , linkedFileName : String
    , userName : String
    , groupName : String
    , fileNamePrefix : String

    -- , typeFlag : Ascii
    }


type alias Ascii =
    Int


{-| defaultMetadata is a dummy MetaData value that you modify
to suit your needs. It contains a lot of boilerplates

Example

metaData= { defaultMetadata | filename = "Test.txt" }

See the definition of MetaData to see what other fields you
may want to modify, or see `/examples/Main.elm`.

-}
defaultMetadata : MetaData
defaultMetadata =
    { filename = "test.txt"
    , mode = blankMode
    , ownerID = 501
    , groupID = 123
    , fileSize = 20
    , lastModificationTime = 1542665285
    , linkIndicator = NormalFile
    , linkedFileName = "bar.txt"
    , userName = "anonymous"
    , groupName = "staff"
    , fileNamePrefix = "abc"

    -- , typeFlag = 0
    }


type alias Mode =
    { user : List FilePermission
    , group : List FilePermission
    , other : List FilePermission
    , system : List SystemInfo
    }


type SystemInfo
    = SUID
    | SGID
    | SVTX


type FilePermission
    = Read
    | Write
    | Execute


type Link
    = NormalFile
    | HardLink
    | SymbolicLink



{- For extracting a tar archive -}


type BlockInfo
    = FileInfo ExtendedMetaData
    | NullBlock
    | Error


type ExtendedMetaData
    = ExtendedMetaData MetaData (Maybe String)


fileSize : ExtendedMetaData -> Int
fileSize (ExtendedMetaData metaData _) =
    metaData.fileSize


fileSizeOfBlockInfo : BlockInfo -> Int
fileSizeOfBlockInfo blockInfo =
    case blockInfo of
        FileInfo extendedMetaData ->
            fileSize extendedMetaData

        NullBlock ->
            0

        Error ->
            0


fileExtension : ExtendedMetaData -> Maybe String
fileExtension (ExtendedMetaData metaData ext) =
    ext


type State
    = Start
    | Processing
    | EndOfData


type alias Output =
    ( BlockInfo, Data )


type alias OutputList =
    List Output



--
-- TESTING
--


{-| A small tar archive for testing purposes
-}
testArchive : Bytes
testArchive =
    encodeTextFiles
        [ ( { defaultMetadata | filename = "one.txt" }, "One" )
        , ( { defaultMetadata | filename = "two.txt" }, "Two" )
        ]
        |> encode



--
-- EXTRACT ARCHIVE
--


{-| Try

> import Tar exposing(..)
> extractArchive testArchive

to test this function

-}
extractArchive : Bytes -> List ( MetaData, Data )
extractArchive bytes =
    bytes
        |> decode decodeFiles
        |> Maybe.withDefault []
        |> List.filter (\x -> List.member (blockInfoOfOuput x) [ NullBlock, Error ] |> not)
        |> List.map simplifyOutput
        |> List.reverse



{- Decoders -}


{-| Example:

> import Bytes.Decode exposing(decode)
> import Tar exposing(..)
> decode decodeFiles testArchive

-}
decodeFiles : Decoder OutputList
decodeFiles =
    Decode.loop ( Start, [] ) fileStep


fileStep : ( State, OutputList ) -> Decoder (Step ( State, OutputList ) OutputList)
fileStep ( state, outputList ) =
    let
        info : OutputList -> State
        info outputList_ =
            case outputList_ of
                [] ->
                    Start

                ( headerInfo_, data ) :: xs ->
                    stateFromBlockInfo headerInfo_
    in
        if state == EndOfData then
            Decode.succeed (Done outputList)
        else
            let
                newState =
                    info outputList
            in
                Decode.map (\output -> Loop ( newState, output :: outputList )) decodeFile


decodeFile : Decoder ( BlockInfo, Data )
decodeFile =
    decodeFirstBlock
        |> Decode.andThen (\blockInfo -> decodeOtherBlocks blockInfo)


decodeFirstBlock : Decoder BlockInfo
decodeFirstBlock =
    Decode.bytes 512
        |> Decode.map (\bytes -> getBlockInfo bytes)


decodeOtherBlocks : BlockInfo -> Decoder ( BlockInfo, Data )
decodeOtherBlocks headerInfo =
    case headerInfo of
        FileInfo (ExtendedMetaData fileRecord maybeExtension) ->
            case maybeExtension of
                Just ext ->
                    if List.member ext textFileExtensions then
                        decodeStringBody (ExtendedMetaData fileRecord maybeExtension)
                    else
                        decodeBinaryBody (ExtendedMetaData fileRecord maybeExtension)

                Nothing ->
                    decodeBinaryBody (ExtendedMetaData fileRecord maybeExtension)

        NullBlock ->
            Decode.succeed ( NullBlock, StringData "NullBlock" )

        Error ->
            Decode.succeed ( Error, StringData "Error" )


decodeStringBody : ExtendedMetaData -> Decoder ( BlockInfo, Data )
decodeStringBody fileHeaderInfo =
    let
        (ExtendedMetaData fileRecord maybeExtension) =
            fileHeaderInfo
    in
        Decode.string (round512 fileRecord.fileSize)
            |> Decode.map (\str -> ( FileInfo fileHeaderInfo, StringData (smashNulls str) ))



-- StringData (String.left fileRecord.fileSize str) )


decodeBinaryBody : ExtendedMetaData -> Decoder ( BlockInfo, Data )
decodeBinaryBody fileHeaderInfo =
    let
        (ExtendedMetaData fileRecord maybeExtension) =
            fileHeaderInfo

        n =
            fileRecord.fileSize
    in
        Decode.bytes (round512 fileRecord.fileSize)
            |> Decode.map (\bytes -> ( FileInfo fileHeaderInfo, BinaryData (take n bytes) ))


{-|

> tf |> getBlockInfo
> { fileName = "test.txt", length = 512 }
-}
getBlockInfo : Bytes -> BlockInfo
getBlockInfo bytes =
    case isHeader_ bytes of
        True ->
            FileInfo (getFileHeaderInfo bytes)

        False ->
            if decode (Decode.string 512) bytes == Just nullString512 then
                NullBlock
            else
                Error


nullString512 : String
nullString512 =
    String.repeat 512 (String.fromChar (Char.fromCode 0))


textFileExtensions =
    [ "text", "txt", "tex", "csv", "html" ]


getFileExtension : String -> Maybe String
getFileExtension str =
    let
        fileParts =
            str
                |> String.split "."
                |> List.reverse
    in
        case List.length fileParts > 1 of
            True ->
                List.head fileParts

            False ->
                Nothing


getFileHeaderInfo : Bytes -> ExtendedMetaData
getFileHeaderInfo bytes =
    let
        fileName =
            getFileName bytes
                |> Maybe.withDefault "unknownFileName"

        metadata =
            { defaultMetadata
                | filename = fileName
                , mode = getMode bytes
                , ownerID = getNumber 108 8 bytes
                , groupID = getNumber 116 8 bytes
                , fileSize = getFileLength bytes
                , lastModificationTime = 0
                , linkIndicator = NormalFile
                , linkedFileName = "foo.txt"
                , userName = getString 265 32 bytes
                , groupName = getString 297 32 bytes
                , fileNamePrefix = getString 345 155 bytes

                -- , typeFlag = getNumber 156 1 bytes
            }
    in
        ExtendedMetaData metadata (getFileExtension fileName)



{- HELPERS FOR DECODING ARCHVES -}


{-| Round integer up to nearest multiple of 512.
-}
round512 : Int -> Int
round512 n =
    let
        residue =
            modBy 512 n
    in
        if residue == 0 then
            n
        else
            n + (512 - residue)


{-| isHeader bytes == True if and only if
bytes has width 512 and contains the
string "ustar"
-}
isHeader : Bytes -> Bool
isHeader bytes =
    if Bytes.width bytes == 512 then
        isHeader_ bytes
    else
        False


isHeader_ : Bytes -> Bool
isHeader_ bytes =
    bytes
        |> decode (Decode.string 512)
        |> Maybe.map (\str -> String.slice 257 262 str == "ustar")
        |> Maybe.withDefault False


getFileName : Bytes -> Maybe String
getFileName bytes =
    bytes
        |> decode (Decode.string 100)
        |> Maybe.map (String.replace (String.fromChar (Char.fromCode 0)) "")


getFileLength : Bytes -> Int
getFileLength bytes =
    bytes
        |> decode (Decode.string 256)
        |> Maybe.map (String.slice 124 136)
        |> Maybe.map (stripLeadingString "0")
        |> Maybe.map String.trim
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0


getNumber : Int -> Int -> Bytes -> Int
getNumber begin length bytes =
    bytes
        |> decode (Decode.string 256)
        |> Maybe.map (String.slice begin (begin + length - 1))
        |> Maybe.map (String.split "")
        |> Maybe.withDefault (List.repeat length "0")
        |> List.map String.toInt
        |> Utility.maybeValues
        |> Octal.integerValueofOctalList


getString : Int -> Int -> Bytes -> String
getString begin length bytes =
    bytes
        |> decode (Decode.string 256)
        |> Maybe.map (String.slice begin (begin + length - 1))
        |> Maybe.withDefault "Oops!"


getMode : Bytes -> Mode
getMode bytes =
    let
        permissions =
            bytes
                |> decode (Decode.string 256)
                |> Maybe.map (String.slice 102 106)
                |> Maybe.map (String.split "")
                |> Maybe.withDefault [ "0", "6", "4", "4" ]
                |> List.map String.toInt
                |> Utility.maybeValues
                |> List.map (Octal.binaryDigits 3)
                |> List.map filePermissionOfBinaryDigits
    in
        addUser permissions nullMode
            |> addGroup permissions
            |> addOther permissions


filePermissionOfBinaryDigits : List Int -> List FilePermission
filePermissionOfBinaryDigits binaryDigits =
    canRead binaryDigits []
        |> canWrite binaryDigits
        |> canExecute binaryDigits


addUser : List (List FilePermission) -> Mode -> Mode
addUser lp mode =
    case Utility.listGetAt 1 lp of
        Just p ->
            { mode | user = p }

        _ ->
            mode


addGroup : List (List FilePermission) -> Mode -> Mode
addGroup lp mode =
    case Utility.listGetAt 2 lp of
        Just p ->
            { mode | group = p }

        _ ->
            mode


addOther : List (List FilePermission) -> Mode -> Mode
addOther lp mode =
    case Utility.listGetAt 3 lp of
        Just p ->
            { mode | other = p }

        _ ->
            mode


{-| Assume fpl is like [1,1,0]}
-}
canRead : List Int -> List FilePermission -> List FilePermission
canRead binaryDigits fpl =
    case Utility.listGetAt 0 binaryDigits of
        Just 1 ->
            Read :: fpl

        _ ->
            fpl


canWrite : List Int -> List FilePermission -> List FilePermission
canWrite binaryDigits fpl =
    case Utility.listGetAt 1 binaryDigits of
        Just 1 ->
            Write :: fpl

        _ ->
            fpl


canExecute : List Int -> List FilePermission -> List FilePermission
canExecute binaryDigits fpl =
    case Utility.listGetAt 2 binaryDigits of
        Just 1 ->
            Execute :: fpl

        _ ->
            fpl


getFileDataFromHeaderInfo : BlockInfo -> MetaData
getFileDataFromHeaderInfo headerInfo =
    case headerInfo of
        FileInfo (ExtendedMetaData fileRecord _) ->
            fileRecord

        _ ->
            defaultMetadata


stateFromBlockInfo : BlockInfo -> State
stateFromBlockInfo blockInfo =
    case blockInfo of
        FileInfo _ ->
            Processing

        NullBlock ->
            EndOfData

        Error ->
            EndOfData


blockInfoOfOuput : Output -> BlockInfo
blockInfoOfOuput ( blockInfo, output ) =
    blockInfo


simplifyOutput : Output -> ( MetaData, Data )
simplifyOutput ( blockInfo, data ) =
    ( getFileDataFromHeaderInfo blockInfo, data )


simplifyOutput2 : Output -> ( MetaData, Data )
simplifyOutput2 ( blockInfo, data ) =
    let
        n =
            fileSizeOfBlockInfo blockInfo
    in
        ( getFileDataFromHeaderInfo blockInfo, take2 n data )


take2 : Int -> Data -> Data
take2 k data =
    case data of
        StringData str ->
            StringData str

        BinaryData bytes_ ->
            BinaryData (take k bytes_)


take : Int -> Bytes -> Bytes
take k bytes =
    Decode.decode (Decode.bytes k) bytes
        |> Maybe.withDefault (Encode.encode (Encode.unsignedInt8 0))



--
-- CREATE ARCHIVE
--


{-| Example:

> data1 = ( { defaultMetadata | filename = "one.txt" }, StringData "One" )
> data2 = ( { defaultMetadata | filename = "two.txt" }, StringData "Two" )
> createArchive [data1, data2]

> createArchive [data1, data2]
> <3072 bytes> : Bytes.Bytes

-}
createArchive : List ( MetaData, Data ) -> Bytes
createArchive dataList =
    encodeFiles dataList |> encode


{-| Example

encodeFiles [(defaultMetadata, "This is a test"), (defaultMetadata, "Lah di dah do day!")] |> Bytes.Encode.encode == <2594 bytes> : Bytes

-}
encodeTextFiles : List ( MetaData, String ) -> Encode.Encoder
encodeTextFiles fileList =
    Encode.sequence
        (List.map (\item -> encodeTextFile (Tuple.first item) (Tuple.second item)) fileList
            ++ [ Encode.string (normalizeString 1024 "") ]
        )


{-|

      Example

      import Tar exposing(defaultMetadata)

      metaData_ =
          defaultMetadata

      metaData1 =
          { metaData_ | filename = "a.txt" }

      content1 =
          "One two three\n"

      metaData2
          { metaData_ | filename = "c.binary" }

      content2 =
          Hex.toBytes "1234" |> Maybe.withDefault (encode (Bytes.Encode.unsignedInt8 0))

      Tar.encodeFiles
          [ ( metaData1, StringData content1 )
          , ( metaData2, BinaryData content2 )
          ]
          |> Bytes.Encode.encode

      Note: `Hex` is found in `jxxcarlson/hex`
-}
encodeFiles : List ( MetaData, Data ) -> Encode.Encoder
encodeFiles fileList =
    Encode.sequence
        (List.map (\( m, d ) -> encodeFile m d) fileList
            ++ [ Encode.string (normalizeString 1024 "") ]
        )


{-| Example:

> encodeTextFile defaultMetadata "Test!" |> encode
> <1024 bytes> : Bytes.Bytes

-}
encodeTextFile : MetaData -> String -> Encode.Encoder
encodeTextFile metaData_ contents =
    let
        metaData =
            { metaData_ | fileSize = String.length contents }
    in
        Encode.sequence
            (case contents == "" of
                True ->
                    [ encodeMetaData metaData ]

                False ->
                    [ encodeMetaData metaData
                    , Encode.string (padContents contents)
                    ]
            )


encodeFile : MetaData -> Data -> Encode.Encoder
encodeFile metaData data =
    case data of
        StringData contents ->
            encodeTextFile metaData contents

        BinaryData bytes ->
            encodeBinaryFile metaData bytes


encodeBinaryFile : MetaData -> Bytes -> Encode.Encoder
encodeBinaryFile metaData_ bytes =
    let
        metaData =
            { metaData_ | fileSize = Bytes.width bytes }
    in
        case metaData.fileSize == 0 of
            True ->
                Encode.sequence [ encodeMetaData metaData ]

            False ->
                Encode.sequence
                    [ encodeMetaData metaData
                    , encodePaddedBytes bytes
                    ]


encodePaddedBytes : Bytes -> Encode.Encoder
encodePaddedBytes bytes =
    let
        paddingWidth =
            modBy 512 (Bytes.width bytes) |> (\x -> 512 - x)
    in
        Encode.sequence
            [ Encode.bytes bytes
            , Encode.sequence <| List.repeat paddingWidth (Encode.unsignedInt8 0)
            ]



--
-- ENCODE METADATA
--


encodeMetaData : MetaData -> Encode.Encoder
encodeMetaData metadata =
    let
        fr =
            preliminaryEncodeMetaData metadata |> encode
    in
        Encode.sequence
            [ Encode.string (normalizeString 100 metadata.filename)
            , encodeMode metadata.mode
            , Encode.sequence [ octalEncoder 6 metadata.ownerID, encodedSpace, encodedNull ]
            , Encode.sequence [ octalEncoder 6 metadata.groupID, encodedSpace, encodedNull ]
            , Encode.sequence [ octalEncoder 11 metadata.fileSize, encodedSpace ]
            , Encode.sequence [ octalEncoder 11 metadata.lastModificationTime, encodedSpace ]
            , Encode.sequence [ CheckSum.sumEncoder fr, encodedNull, encodedSpace ]
            , linkEncoder metadata.linkIndicator
            , Encode.string (normalizeString 100 metadata.linkedFileName)
            , Encode.sequence [ Encode.string "ustar", encodedNull ]
            , Encode.string "00"
            , Encode.string (normalizeString 32 metadata.userName)
            , Encode.string (normalizeString 32 metadata.groupName)
            , Encode.sequence [ octalEncoder 7 0, encodedSpace ]
            , Encode.sequence [ octalEncoder 7 0, encodedSpace ]
            , Encode.string (normalizeString 167 metadata.fileNamePrefix)
            ]


preliminaryEncodeMetaData : MetaData -> Encode.Encoder
preliminaryEncodeMetaData metadata =
    Encode.sequence
        [ Encode.string (normalizeString 100 metadata.filename)
        , encodeMode metadata.mode
        , Encode.sequence [ octalEncoder 6 metadata.ownerID, encodedSpace, encodedNull ]
        , Encode.sequence [ octalEncoder 6 metadata.groupID, encodedSpace, encodedNull ]
        , Encode.sequence [ octalEncoder 11 metadata.fileSize, encodedSpace ]
        , Encode.sequence [ octalEncoder 11 metadata.lastModificationTime, encodedSpace ]
        , Encode.string "        "
        , encodedSpace -- slinkEncoder fileRecord.linkIndicator
        , Encode.string (normalizeString 100 metadata.linkedFileName)
        , Encode.sequence [ Encode.string "ustar", encodedNull ]
        , Encode.string "00"
        , Encode.string (normalizeString 32 metadata.userName)
        , Encode.string (normalizeString 32 metadata.groupName)
        , Encode.sequence [ octalEncoder 7 0, encodedSpace ]
        , Encode.sequence [ octalEncoder 7 0, encodedSpace ]
        , Encode.string (normalizeString 167 metadata.fileNamePrefix)
        ]


linkEncoder : Link -> Encode.Encoder
linkEncoder link =
    case link of
        NormalFile ->
            Encode.string "0"

        HardLink ->
            Encode.string "1"

        SymbolicLink ->
            Encode.string "2"


encodeFilePermissions : List FilePermission -> Encode.Encoder
encodeFilePermissions fps =
    fps
        |> List.map encodeFilePermission
        |> List.sum
        |> (\x -> x + 48)
        |> Encode.unsignedInt8


encodeSystemInfo : SystemInfo -> Int
encodeSystemInfo si =
    case si of
        SVTX ->
            1

        SGID ->
            2

        SUID ->
            4


encodeSystemInfos : List SystemInfo -> Encode.Encoder
encodeSystemInfos sis =
    sis
        |> List.map encodeSystemInfo
        |> List.sum
        |> (\x -> x + 48)
        |> Encode.unsignedInt8


encodeMode : Mode -> Encode.Encoder
encodeMode mode =
    Encode.sequence
        [ Encode.unsignedInt8 48
        , Encode.unsignedInt8 48
        , Encode.unsignedInt8 48
        , encodeFilePermissions mode.user
        , encodeFilePermissions mode.group
        , encodeFilePermissions mode.other
        , Encode.unsignedInt8 32 -- encodeSystemInfos mode.system
        , Encode.unsignedInt8 0
        ]


encodeInt8 : Int -> Encode.Encoder
encodeInt8 n =
    Encode.sequence
        [ Encode.unsignedInt32 BE 0
        , Encode.unsignedInt32 BE n
        ]


encodeInt12 : Int -> Encode.Encoder
encodeInt12 n =
    Encode.sequence
        [ Encode.unsignedInt32 BE 0
        , Encode.unsignedInt32 BE 0
        , Encode.unsignedInt32 BE n
        ]



{- HELPERS FOR ENCODEING FILES -}


{-| Add zeros at end of file to make its length a multiple of 512.
-}
padContents : String -> String
padContents str =
    let
        paddingLength =
            modBy 512 (String.length str) |> (\x -> 512 - x)

        nullString =
            String.fromChar (Char.fromCode 0)

        padding =
            String.repeat paddingLength nullString
    in
        str ++ padding


encodedSpace =
    Encode.string " "


encodedZero =
    Encode.string "0"


encodedNull =
    Encode.string (String.fromChar (Char.fromCode 0))


blankMode =
    { user = [ Read, Write, Execute ]
    , group = [ Read, Write ]
    , other = [ Read ]
    , system = [ SGID ]
    }


nullMode =
    { user = []
    , group = []
    , other = []
    , system = []
    }


encodeFilePermission : FilePermission -> Int
encodeFilePermission fp =
    case fp of
        Read ->
            4

        Write ->
            2

        Execute ->
            1



--
-- HELPERS
--


stripLeadingString : String -> String -> String
stripLeadingString lead str =
    str
        |> String.split ""
        |> stripLeadingElement lead
        |> String.join ""


stripLeadingElement : a -> List a -> List a
stripLeadingElement lead list =
    case list of
        [] ->
            []

        [ x ] ->
            if lead == x then
                []
            else
                [ x ]

        x :: xs ->
            if lead == x then
                stripLeadingElement lead xs
            else
                x :: xs


{-| return string of length n, truncated
if necessary, and then padded, if neccessary,
with 0's on the right.
-}
normalizeString : Int -> String -> String
normalizeString n str =
    str |> String.left n |> String.padRight n (Char.fromCode 0)


smashNulls : String -> String
smashNulls str =
    String.replace (String.fromChar (Char.fromCode 0)) "" str

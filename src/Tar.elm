module Tar exposing (Data(..), encodeTextFiles, encodeFiles, defaultFileRecord)

{-| With this package you can create tar archives. Use

    ```
    encodeFiles : List (FileRecord, String) -> Encode.Encoder

    fileRecord1 =
        { Tar.defaultFileRecord | filename = "test123.txt" }

    content1 =
        "This is a test (ho ho ho).\nIt is a frabjous day!"

    bytes = Tar.encodeFiles [ ( fileRecord1, content1 ) ] |> Bytes.Encode.encode

    File.Download.bytes ("test.tar") "application/x-tar" bytes
     ```

Just put more pairs `(fileRecord, content)` in the list above to
archive more files.

@docs encodeFiles, defaultFileRecord

-}

-- (encodeFiles, defaultFileRecord)

import Bytes exposing (..)
import Bytes.Decode as Decode exposing (decode)
import Bytes.Encode as Encode exposing (encode)
import Time exposing (Posix)
import Char
import CheckSum
import Octal exposing (octalEncoder)


type alias FileRecord =
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



--
-- ENCODE FILES
--


{-|

> encodeFiles [(defaultFileRecord, "This is a test"), (defaultFileRecord, "Lah di dah do day!")] |> encode
> <2594 bytes> : Bytes
-}
encodeTextFiles : List ( FileRecord, String ) -> Encode.Encoder
encodeTextFiles fileList =
    Encode.sequence
        ((List.map (\item -> encodeTextFile (Tuple.first item) (Tuple.second item)) fileList)
            ++ [ Encode.string (normalizeString 1024 "") ]
        )


encodeFiles : List ( FileRecord, Data ) -> Encode.Encoder
encodeFiles fileList =
    Encode.sequence
        ((List.map (\item -> encodeFile (Tuple.first item) (Tuple.second item)) fileList)
            ++ [ Encode.string (normalizeString 1024 "") ]
        )


type Data
    = StringData String
    | BinaryData Bytes


encodeTextFile : FileRecord -> String -> Encode.Encoder
encodeTextFile fileRecord_ contents =
    let
        fileRecord =
            { fileRecord_ | fileSize = String.length contents }
    in
        Encode.sequence
            [ encodeFileRecord fileRecord
            , Encode.string (padContents contents)
            ]


encodeFile : FileRecord -> Data -> Encode.Encoder
encodeFile fileRecord_ data =
    case data of
        StringData contents ->
            encodeTextFile fileRecord_ contents

        BinaryData bytes ->
            encodeBinaryFile fileRecord_ bytes


encodeBinaryFile : FileRecord -> Bytes -> Encode.Encoder
encodeBinaryFile fileRecord_ bytes =
    let
        fileRecord =
            { fileRecord_ | fileSize = Bytes.width bytes }
    in
        Encode.sequence
            [ encodeFileRecord fileRecord
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


{-|

> encodeFileRecord defaultFileRecord |> encode |> Hex.fromBytes
> encodeFileRecord defaultFileRecord |> encode |> width
-}
defaultFileRecord : FileRecord
defaultFileRecord =
    FileRecord
        "test.txt"
        blankMode
        501
        20
        123
        1542665285
        NormalFile
        ""
        "anonymous"
        "staff"
        ""


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



--
-- ENCODE FILE RECORD
--


encodedSpace =
    Encode.string " "


encodedZero =
    Encode.string "0"


encodedNull =
    Encode.string (String.fromChar (Char.fromCode 0))


encodeFileRecord : FileRecord -> Encode.Encoder
encodeFileRecord fileRecord =
    let
        fr =
            preliminaryEncodeFileRecord fileRecord |> encode
    in
        Encode.sequence
            [ Encode.string (normalizeString 100 fileRecord.filename)
            , encodeMode fileRecord.mode
            , Encode.sequence [ octalEncoder 6 fileRecord.ownerID, encodedSpace, encodedNull ]
            , Encode.sequence [ octalEncoder 6 fileRecord.groupID, encodedSpace, encodedNull ]
            , Encode.sequence [ octalEncoder 11 fileRecord.fileSize, encodedSpace ]
            , Encode.sequence [ octalEncoder 11 fileRecord.lastModificationTime, encodedSpace ]
            , Encode.sequence [ CheckSum.sumEncoder fr, encodedNull, encodedSpace ]
            , linkEncoder fileRecord.linkIndicator
            , Encode.string (normalizeString 100 fileRecord.linkedFileName)
            , Encode.sequence [ Encode.string "ustar", encodedNull ]
            , Encode.string "00"
            , Encode.string (normalizeString 32 fileRecord.userName)
            , Encode.string (normalizeString 32 fileRecord.groupName)
            , Encode.sequence [ octalEncoder 6 0, encodedSpace ]
            , Encode.sequence [ encodedNull, octalEncoder 6 0, encodedSpace ]
            , Encode.string (normalizeString 168 fileRecord.fileNamePrefix)
            ]


preliminaryEncodeFileRecord : FileRecord -> Encode.Encoder
preliminaryEncodeFileRecord fileRecord =
    Encode.sequence
        [ Encode.string (normalizeString 100 fileRecord.filename)
        , encodeMode fileRecord.mode
        , Encode.sequence [ octalEncoder 6 fileRecord.ownerID, encodedSpace, encodedNull ]
        , Encode.sequence [ octalEncoder 6 fileRecord.groupID, encodedSpace, encodedNull ]
        , Encode.sequence [ octalEncoder 11 fileRecord.fileSize, encodedSpace ]
        , Encode.sequence [ octalEncoder 11 fileRecord.lastModificationTime, encodedSpace ]
        , Encode.string "        "
        , encodedSpace -- slinkEncoder fileRecord.linkIndicator
        , Encode.string (normalizeString 100 fileRecord.linkedFileName)
        , Encode.sequence [ Encode.string "ustar", encodedNull ]
        , Encode.string "00"
        , Encode.string (normalizeString 32 fileRecord.userName)
        , Encode.string (normalizeString 32 fileRecord.groupName)
        , Encode.sequence [ octalEncoder 6 0, encodedSpace ]
        , Encode.sequence [ encodedNull, octalEncoder 6 0, encodedSpace ]
        , Encode.string (normalizeString 168 fileRecord.fileNamePrefix)
        ]



--
-- ENCODERS
--


linkEncoder : Link -> Encode.Encoder
linkEncoder link =
    case link of
        NormalFile ->
            Encode.string "0"

        HardLink ->
            Encode.string "1"

        SymbolicLink ->
            Encode.string "2"


blankMode =
    Mode [ Read, Write ] [ Read ] [ Read ] [ SGID ]


encodeFilePermission : FilePermission -> Int
encodeFilePermission fp =
    case fp of
        Read ->
            4

        Write ->
            2

        Execute ->
            1


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


{-| return string of length n, truncated
if necessary, and then padded, if neccessary,
with 0's on the right.
-}
normalizeString : Int -> String -> String
normalizeString n str =
    str |> String.left n |> String.padRight n (Char.fromCode 0)



{- NOTES -}
{- @gabber

   / untested
   myzip : List Bytes -> Bytes.Encode.Encoder
   myzip files =
       let
           file_encoder : Bytes-> Bytes.Encode.Encoder
           file_encoder file =
               Bytes.Encode.sequence
                   [ Bytes.Encode.unsignedInt32 Bytes.Encode.BE <| Bytes.width file
                   , Bytes.Encode.bytes file
                   ]
       in
       Encode.sequence <|
           [ Bytes.Encode.unsignedInt8 <| List.length files ]
           ++ List.map file_encoder files

-}

module Tar exposing
    ( createArchive, extractArchive
    , Data(..), MetaData, defaultMetaData
    , encodeFiles, encodeTextFile, encodeTextFiles
    )

{-| For more details, see the README. See also the demo app `./examples/Main.elm`

@docs createArchive, extractArchive

@docs Data, MetaData, defaultMetaData


## Encoders

Convenient for integration with other `Bytes.Encode.Encoder`s.

@docs encodeFiles, encodeTextFile, encodeTextFiles

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..), decode)
import Bytes.Encode as Encode exposing (encode)
import Char
import Octal
import Set exposing (Set)
import String.Graphemes



--
-- TYPES
--


{-| Use `StringData String` for text data, `BinaryData Bytes` for binary data:

    import Bytes.Encode as Encode

    StringData "This is a test"

    BinaryData (Encode.encode (Encode.string "foo"))

-}
type Data
    = StringData String
    | BinaryData Bytes


{-| A MetaData value contains the information, e.g.,
file name and file length, needed to construct the header
for a file in the tar archive. You may use `defaultMetaData` as
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
    }


{-| Defined as

    defaultMetaData : MetaData
    defaultMetaData =
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
        }

Example usage:

    myMetaData =
        { defaultMetaData | filename = "Test.txt" }

-}
defaultMetaData : MetaData
defaultMetaData =
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
    }


type alias Mode =
    { owner : FilePermission
    , group : FilePermission
    , other : FilePermission
    , setUserID : Bool
    , setGroupID : Bool
    }


type alias FilePermission =
    { read : Bool
    , write : Bool
    , execute : Bool
    }


type Link
    = NormalFile
    | HardLink
    | SymbolicLink



--
-- EXTRACT ARCHIVE
--


{-| Decode an archive into its constituent files.
-}
extractArchive : Bytes -> List ( MetaData, Data )
extractArchive bytes =
    decodeArchive bytes


decodeTextAsStringHelp : ( MetaData, Bytes ) -> ( MetaData, Data )
decodeTextAsStringHelp ( meta, bytes ) =
    case getFileExtension meta.filename of
        Nothing ->
            -- cannot determine filetype; default to binary
            ( meta, BinaryData (takeBytes meta.fileSize bytes) )

        Just extension ->
            if Set.member extension textFileExtensions then
                case Decode.decode (Decode.string meta.fileSize) bytes of
                    Just str ->
                        ( meta, StringData str )

                    Nothing ->
                        ( meta, StringData "" )

            else
                ( meta, BinaryData (takeBytes meta.fileSize bytes) )


decodeArchive : Bytes -> List ( MetaData, Data )
decodeArchive bytes =
    case Decode.decode (decodeFiles (Bytes.width bytes // 512)) bytes of
        Just v ->
            List.map decodeTextAsStringHelp v

        Nothing ->
            []



-- DECODERS


decodeFiles : Int -> Decoder (List ( MetaData, Bytes ))
decodeFiles n =
    Decode.loop { state = Start, blocksRemaining = n, files = [] } fileStep


type State
    = Start
    | Processing MetaData (List Bytes)


type alias Accum =
    { state : State
    , blocksRemaining : Int
    , files : List ( MetaData, Bytes )
    }


fileStep : Accum -> Decoder (Step Accum (List ( MetaData, Bytes )))
fileStep { state, blocksRemaining, files } =
    let
        build meta blocks =
            ( meta
            , List.reverse blocks
                |> List.map Encode.bytes
                |> Encode.sequence
                |> Encode.encode
            )
    in
    if blocksRemaining > 0 then
        Decode.bytes 512
            |> Decode.andThen
                (\block ->
                    case Decode.decode decodeMetaData block of
                        Nothing ->
                            case state of
                                Start ->
                                    -- first block must be a metadata header
                                    Decode.fail

                                Processing meta blocks ->
                                    Decode.succeed
                                        { state = Processing meta (block :: blocks)
                                        , blocksRemaining = blocksRemaining - 1
                                        , files = files
                                        }

                        Just { metadata, checksum } ->
                            -- the checksum is the sum of the header bytes, where the
                            -- checksum field is taken to be 8 spaces
                            -- so here we calculate the byte sum of `block`, and subtract the
                            -- sum of the checksum bytes
                            if checksum.value == sumBytes block - checksum.byteSum then
                                Decode.succeed
                                    { state = Processing metadata []
                                    , blocksRemaining = blocksRemaining - 1
                                    , files =
                                        case state of
                                            Start ->
                                                files

                                            Processing meta blocks ->
                                                build meta blocks :: files
                                    }

                            else
                                -- invalid checksum
                                Decode.fail
                )
            |> Decode.map Decode.Loop

    else
        case state of
            Start ->
                Decode.succeed (Decode.Done (List.reverse files))

            Processing meta blocks ->
                Decode.succeed (Decode.Done (List.reverse (build meta blocks :: files)))


textFileExtensions : Set String
textFileExtensions =
    Set.fromList [ "text", "txt", "tex", "csv", "html" ]


getFileExtension : String -> Maybe String
getFileExtension str =
    let
        fileParts =
            str
                |> String.split "."
                |> List.reverse
    in
    if List.length fileParts > 1 then
        List.head fileParts

    else
        Nothing



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


modeBits :
    { gid : Int
    , group : { execute : Int, read : Int, write : Int }
    , other : { execute : Int, read : Int, write : Int }
    , owner : { execute : Int, read : Int, write : Int }
    , reserved : Int
    , uid : Int
    }
modeBits =
    { uid = 2048
    , gid = 1024

    -- reserved/unused
    , reserved =
        512
    , owner =
        { read = 256
        , write = 128
        , execute = 64
        }
    , group =
        { read = 32
        , write = 16
        , execute = 8
        }
    , other =
        { read = 4
        , write = 2
        , execute = 1
        }
    }


decodeMode : Decoder Mode
decodeMode =
    Octal.decode 8
        |> Decode.map
            (\flags ->
                let
                    isSet : Int -> Int -> Bool
                    isSet bit pattern =
                        Bitwise.and bit pattern /= 0
                in
                { owner =
                    { read = isSet modeBits.owner.read flags
                    , write = isSet modeBits.owner.write flags
                    , execute = isSet modeBits.owner.execute flags
                    }
                , group =
                    { read = isSet modeBits.group.read flags
                    , write = isSet modeBits.group.write flags
                    , execute = isSet modeBits.group.execute flags
                    }
                , other =
                    { read = isSet modeBits.other.read flags
                    , write = isSet modeBits.other.write flags
                    , execute = isSet modeBits.other.execute flags
                    }
                , setUserID = isSet modeBits.uid flags
                , setGroupID = isSet modeBits.gid flags
                }
            )



--
-- CREATE ARCHIVE
--


{-| Example:

    data1 : ( MetaData, Data )
    data1 =
        ( { defaultMetaData | filename = "one.txt" }
        , StringData "One"
        )

    data2 : ( MetaData, Data )
    data2 =
        ( { defaultMetaData | filename = "two.txt" }
        , StringData "Two"
        )

    createArchive [data1, data2]

-}
createArchive : List ( MetaData, Data ) -> Bytes
createArchive dataList =
    encodeFiles dataList |> encode


{-| Per the spec:

> At the end of the archive file there are two 512-byte blocks filled with binary zeros as an end-of-file marker.

-}
endOfFileMarker : Encode.Encoder
endOfFileMarker =
    String.repeat 1024 "\u{0000}"
        |> Encode.string


{-| Encoder for a list of files

    import Bytes
    import Bytes.Encode as Encode
    import Tar exposing (defaultMetaData)

    metaData1 : Tar.MetaData
    metaData1 =
        { defaultMetaData | filename = "a.txt" }

    content1 : String
    content1 =
        "One two three\n"

    metaData2 : Tar.MetaData
    metaData2
        { defaultMetaData | filename = "c.binary" }

    content2 : Bytes.Bytes
    content2 =
        "1345"
          |> Encode.string
          |> Encode.encode

    result : Bytes
    result =
        [ ( metaData1, Tar.StringData content1 )
        , ( metaData2, Tar.BinaryData content2 )
        ]
        |> Tar.encodeFiles
        |> Bytes.Encode.encode

-}
encodeFiles : List ( MetaData, Data ) -> Encode.Encoder
encodeFiles fileList =
    let
        folder ( metadata, string ) accum =
            encodeFile metadata string :: accum
    in
    List.foldr folder [ endOfFileMarker ] fileList
        |> Encode.sequence


{-| -}
encodeTextFile : MetaData -> String -> Encode.Encoder
encodeTextFile metaData contents =
    encodeBinaryFile metaData (Encode.encode (Encode.string contents))


{-| -}
encodeTextFiles : List ( MetaData, String ) -> Encode.Encoder
encodeTextFiles fileList =
    let
        folder ( metadata, string ) accum =
            encodeTextFile metadata string :: accum
    in
    List.foldr folder [ endOfFileMarker ] fileList
        |> Encode.sequence


encodeFile : MetaData -> Data -> Encode.Encoder
encodeFile metaData data =
    case data of
        StringData contents ->
            encodeTextFile metaData contents

        BinaryData bytes ->
            encodeBinaryFile metaData bytes


encodeBinaryFile : MetaData -> Bytes -> Encode.Encoder
encodeBinaryFile metaData bytes =
    let
        width =
            Bytes.width bytes
    in
    case width of
        0 ->
            encodeMetaData { metaData | fileSize = width }

        _ ->
            Encode.sequence
                [ encodeMetaData { metaData | fileSize = width }
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
        , Encode.bytes (takeBytes paddingWidth nullBlock)
        ]


nullBlock : Bytes
nullBlock =
    List.repeat (512 // 4) (Encode.unsignedInt32 Bytes.BE 0)
        |> Encode.sequence
        |> Encode.encode



--
-- ENCODE METADATA
--


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap later first =
    Decode.map2 (\f x -> f x) first later


skip : Decoder x -> Decoder a -> Decoder a
skip later first =
    Decode.map2 (\f _ -> f) first later


decodeMetaData : Decoder { metadata : MetaData, checksum : Checksum }
decodeMetaData =
    let
        helper name mode uid gid size mtime checksum linkname magic uname gname prefix =
            if String.startsWith "ustar" magic then
                Decode.succeed
                    { metadata =
                        { filename = name
                        , mode = mode
                        , ownerID = uid
                        , groupID = gid
                        , fileSize = size
                        , lastModificationTime = mtime
                        , linkIndicator = NormalFile
                        , linkedFileName = linkname
                        , userName = uname
                        , groupName = gname
                        , fileNamePrefix = prefix
                        }
                    , checksum = checksum
                    }

            else
                Decode.fail
    in
    Decode.succeed helper
        |> andMap (cstring 100)
        |> andMap decodeMode
        |> andMap (Octal.decode 8)
        |> andMap (Octal.decode 8)
        |> andMap (Octal.decode 12)
        |> andMap (Octal.decode 12)
        -- center
        |> andMap decodeChecksum
        -- bottom
        |> skip Decode.unsignedInt8
        |> andMap (cstring 100)
        |> andMap (cstring 6)
        |> skip (Decode.bytes 2)
        |> andMap (cstring 32)
        |> andMap (cstring 32)
        |> skip (Octal.decode 8)
        |> skip (Octal.decode 8)
        |> andMap (cstring 131)
        |> skip (Decode.bytes 12)
        |> skip (Decode.bytes 12)
        |> Decode.andThen identity


type alias Checksum =
    { value : Int
    , byteSum : Int
    }


decodeChecksum : Decoder Checksum
decodeChecksum =
    Decode.bytes 8
        |> Decode.andThen
            (\bytes ->
                case Decode.decode (Octal.decode 7) bytes of
                    Just value ->
                        Decode.succeed { value = value, byteSum = sumBytes bytes - 8 * Char.toCode ' ' }

                    Nothing ->
                        Decode.fail
            )


{-| Encode metadata

The metadata contains a checksum, based on the other fields of the metadata header.
Therefore we must first encode those parts (the checksum field is 8 spaces in this case),
sum its bytes, then re-encode the full header with the checksum.

-}
encodeMetaData : MetaData -> Encode.Encoder
encodeMetaData metadata =
    let
        metaDataTop : Bytes
        metaDataTop =
            [ Encode.string (normalizeString 100 metadata.filename)
            , encodeMode metadata.mode
            , Encode.sequence [ Octal.encode 8 metadata.ownerID ]
            , Encode.sequence [ Octal.encode 8 metadata.groupID ]
            , Encode.sequence [ Octal.encode 12 metadata.fileSize ]
            , Encode.sequence [ Octal.encode 12 metadata.lastModificationTime ]
            ]
                |> Encode.sequence
                |> Encode.encode

        metaDataBottom : Bytes
        metaDataBottom =
            [ linkEncoder metadata.linkIndicator
            , Encode.string (normalizeString 100 metadata.linkedFileName)
            , Encode.string (normalizeString 6 "ustar")
            , Encode.string "00"
            , Encode.string (normalizeString 32 metadata.userName)
            , Encode.string (normalizeString 32 metadata.groupName)
            , Encode.sequence [ Octal.encode 8 0 ]
            , Encode.sequence [ Octal.encode 8 0 ]
            , Encode.string (normalizeString 167 metadata.fileNamePrefix)
            ]
                |> Encode.sequence
                |> Encode.encode

        preliminary : List Encode.Encoder
        preliminary =
            [ Encode.bytes metaDataTop
            , Encode.string (String.repeat 8 " ")
            , Encode.bytes metaDataBottom
            ]

        checksum : Int
        checksum =
            preliminary
                |> Encode.sequence
                |> Encode.encode
                |> sumBytes
    in
    Encode.sequence
        [ Encode.bytes metaDataTop
        , Octal.encode 7 checksum
        , Encode.string " "
        , Encode.bytes metaDataBottom
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


encodeMode : Mode -> Encode.Encoder
encodeMode mode =
    let
        isSet test bit =
            if test then
                bit

            else
                0

        bitflags : Int
        bitflags =
            0
                -- user
                |> Bitwise.or (isSet mode.owner.read modeBits.owner.read)
                |> Bitwise.or (isSet mode.owner.write modeBits.owner.write)
                |> Bitwise.or (isSet mode.owner.execute modeBits.owner.execute)
                -- group
                |> Bitwise.or (isSet mode.group.read modeBits.group.read)
                |> Bitwise.or (isSet mode.group.write modeBits.group.write)
                |> Bitwise.or (isSet mode.group.execute modeBits.group.execute)
                -- other
                |> Bitwise.or (isSet mode.other.read modeBits.other.read)
                |> Bitwise.or (isSet mode.other.write modeBits.other.write)
                |> Bitwise.or (isSet mode.other.execute modeBits.other.execute)
                -- ids
                |> Bitwise.or (isSet mode.setUserID modeBits.uid)
                |> Bitwise.or (isSet mode.setGroupID modeBits.gid)
    in
    Octal.encode 8 bitflags



-- HELPERS FOR ENCODING FILES


blankMode : Mode
blankMode =
    { owner = { read = True, write = True, execute = True }
    , group = { read = True, write = True, execute = False }
    , other = { read = True, write = False, execute = False }
    , setUserID = False
    , setGroupID = False
    }


nullMode : Mode
nullMode =
    { owner = { read = False, write = False, execute = False }
    , group = { read = False, write = False, execute = False }
    , other = { read = False, write = False, execute = False }
    , setUserID = False
    , setGroupID = False
    }



--
-- HELPERS
--


{-| Encode a c-style null-delimited string of a specific length.

  - the string capped a `length - 1`
  - the string is padded with the null character to the desired length

We must be careful with unicode characters here: for `String.length` all characters
are the same width (namely 1), but when encoded as utf-8 (with `Encode.string`), some characters
can take more than one byte.

Functions in the `String` module implicitly use the character length. We use `String.Graphemes` here
to ensure the string is within limits, but in fact a valid string (so no "half" characters).

-}
normalizeString : Int -> String -> String
normalizeString desiredLength str =
    case desiredLength of
        0 ->
            -- just to be safe. otherwise unbounded recursion
            ""

        _ ->
            let
                dropped =
                    str
                        -- first `desiredLength` characters
                        -- but this function should produce
                        -- `desiredLength` bytes, not characters
                        |> String.left desiredLength
                        -- so this step is required
                        |> dropRightLoop (desiredLength - 1)

                paddingSize =
                    desiredLength - Encode.getStringWidth dropped
            in
            dropped ++ String.repeat paddingSize "\u{0000}"


dropRightLoop : Int -> String -> String
dropRightLoop desiredLength str =
    if Encode.getStringWidth str > desiredLength then
        dropRightLoop desiredLength (String.Graphemes.dropRight 1 str)

    else
        str


smashNulls : String -> String
smashNulls str =
    String.replace (String.fromChar (Char.fromCode 0)) "" str


cstring : Int -> Decoder String
cstring n =
    Decode.map smashNulls (Decode.string n)


takeBytes : Int -> Bytes -> Bytes
takeBytes k bytes =
    case Decode.decode (Decode.bytes k) bytes of
        Just v ->
            v

        Nothing ->
            bytes


{-| Sum all the bytes in a `Bytes`.
-}
sumBytes : Bytes -> Int
sumBytes bytes =
    let
        decoder =
            Decode.loop { remaining = Bytes.width bytes, accum = 0 } sumBytesHelp
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

# Elm-Tar

With this package you can both create and extract tar archives using

```elm
createArchive : List (Metadata, Data) -> Bytes

extractArchive : Bytes -> List (Metadata, Data)
```

## Example

To give a simple example, we make define some binary data
and check that it is what we think it is. 
Next, we create an archive consisting of one text file and one binary file:

```elm
import Hex.Convert          -- from jxxcarlson/hex
import Bytes                -- from elm/bytes
import Bytes.Encode         -- from elm/bytes
import Tar exposing(defaultMetadata)

-- Bytes example 

metadata : Tar.Metadata
metadata = { defaultMetadata | filename = "test123.txt" }

bytes : Bytes.Bytes
bytes = 
    Bytes.Encode.unsignedInt32 Bytes.BE 0x0001A1FF
        |> Bytes.Encode.encode

Hex.Convert.toString bytes
    --> "0001A1FF" 

-- String example 

text : String
text = "This is a test (ho ho ho).\nIt is a frabjous day!"

metadata2 : Tar.Metadata
metadata2 = { defaultMetadata | filename = "foo.binary" }

-- Create archive

archive : Bytes.Bytes
archive = 
    Tar.createArchive 
        [ ( metadata, Tar.StringData text )
        , ( metadata2, Tar.BinaryData bytes ) 
        ]

Bytes.width archive 
    --> 3072


-- usage of `elm/file` 
download : Cmd msg 
download = 
    File.Download.bytes "test.tar" 
        "application/x-tar" 
        archive
```

## Types

The `Data` type discriminates between string and binary data:

```elm
type Data
    = StringData String
    | BinaryData Bytes
```

The `Metadata` type is complex and reflects the official tar specification:

```elm
type alias Metadata =
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
```
The `mode` carries Unix file permissions, e.g., 644.  The fileNamePrefix
can be thought of as the name of the folder/directory in which the files
to be processed live.

Because filling out this record is something a pain, a `defaultMetadata : Metadata`
value is provided.  It can be modified as needed.


## Metadata notes

(1) There is an unresolved problem of how to properly encode the metadata
of text files so that `extractArchive` knows to decode the content
as a string.  At the moment, archived files whose content is to be
decoded as a string are recognized by their file extension.  The
admissible text files have extension  in the list

```elm
[ "text", "txt", "tex", "csv" ]
```

This is an unsatisfactory solution, but at the moment, I don't
know how to get around it.

(2) The `fileNamePrefix` in the metadata is the name of the folder
into which the files willl be decoded.


## Improvements

As Runar Furenes (@ruf) pointed out to me, there was an asymmetry in the
way metadata was treated: not the same for `extractArchive` as for `createArchive`.
This sort of thing is almost always a sign of bad design and poor esthetic judgement.
I've fixed it, so the type signatures are now as above. Much better!




## Testing

Well, we really do need some tests, as @ruf also noted. [Thanks Runar!]  We do have some rudimentary ones now:

```bash
   > import Tests exposing(..)

   > checkListAsString stringTestData
     [(0,True),(1,True),(2,True)]

   > checkListAsBinary binaryTestData
   > [(0,True),(1,True)] : List ( Int, Bool )
```
See the module `Test` to see what these results
mean.  Basically, `(k, True)` means that the k-th test passes.  More detalis in `Tests.md`.


## Demo app

For a demo, run `elm make Main.elm` in `/examples`, then click on the resulting `index.html` file.

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

## Credits

Thanks to Folkert de Vries for many code improvements including 
bug fixes (handling of unicode characters) and
performance optimizations.

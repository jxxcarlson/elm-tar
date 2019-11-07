# Elm-Tar

With this package you can both create and extract tar archives using

```elm
createArchive : List (Metadata, Data) -> Bytes

extractArchive : Bytes -> List ( Metadata, Data )
```

## Types

The `Data` type discriminates between string and binary  data:

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

## Example

To give a simple example, we make define some binary data
and check that it is what we think it is:

```
> import Hex                  -- from jxxcarlson/hex
> import Bytes.Encode as E    -- from elm/bytes

> null = E.encode (E.unsignedInt8 0)
<1 bytes> : Bytes.Bytes

> bytes = Hex.toBytes "0001A1FF" |> Maybe.withDefault null
<4 bytes> : Bytes.Bytes

> Hex.fromBytes bytes
"0001A1FF" : String
```

Next, we create an archive consisting of one text file and one binary file:
```
import Tar exposing(..)

metadata = { defaultMetadata | filename = "test123.txt" }

text = "This is a test (ho ho ho).\nIt is a frabjous day!"

metadata2 = { defaultMetadata | filename = "foo.binary" }

> archive = createArchive [ ( metadata, StringData text ), ( metadata2, BinaryData bytes ) ]
<3072 bytes> : Bytes.Bytes

File.Download.bytes "test.tar" "application/x-tar" archive
<internals> : Cmd msg -- You can't do this one in the repl.
```

## Metadata notes

(1) There is an unresolved problem of how to properly encode the metadata
of text files so that `extractArchive` knows to decode the content
as a string.  At the moment, archived files whose content is to be
decoded as a string are recognized by their file extension.  The
admissible text files have extension  in the list

```
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

```
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

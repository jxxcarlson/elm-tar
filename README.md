# Elm-Tar

With this package you can both create and extract tar archives using
```
   createArchive List (Metadata, Data) -> Bytes

   extractArchive : Bytes -> List ( MetaData, Data )
```

## Types  

The `Data` type discriminates between string and binary  data:
```
type Data
    = StringData String
    | BinaryData Bytes
```

The `MetaData` type is complex and reflects the official tar specification:

```
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
    , typeFlag : Ascii
    }
```
The `mode` carries Unix file permissions, e.g., 644.  The fileNamePrefix
can be thought of as the name of the folder/directory in which the files
to be processed live.

Because filling out this record is something a pain, a `defaultMetadata : MetaData`
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

metadata1 = { defaultMetadata | filename = "test123.txt" }

text = "This is a test (ho ho ho).\nIt is a frabjous day!"

metadata2 = { defaultMetadata | filename = "foo.binary" }

> archive = createArchive [
      ( metadata1, StringData text )
    , ( metadata2, BinaryData bytes )
  ]
<3072 bytes> : Bytes.Bytes

File.Download.bytes "test.tar" "application/x-tar" archive
<internals> : Cmd msg -- You can't do this one in the repl.
```


## Improvements

As Runar Furenes (@ruf) pointed out to me, there was an asymmetry in the
way metadata was treated: not the same for `extractArchive` as for `createArchive`.
This sort of thing is almost always a sign of bad design and poor esthetic judgement.
I've fixed it, so the type signatures are now as above. Much better!




## Testing  

Well, we really do need some tests, as @ruf (Runar Furenes) pointed out to me. [Thanks Runar!]

Runar did some fuzz testing which revealed that creating an archive and then extracting the data does not always give back the data you started with ... there are sometimes trailing nulls.  I see them using `extractArchive` following
`createArchive`, which should be an implementation of the identity function. So far I have not observed this error
when creating an archive, downloading it, and then extracting it using `tar -xvf`.

I hope to get to the bottom of this very soon.  Meanwhile, as an aid to finding this bug and for just testing in general, I've added a `Test` module.  It doesn't have actual tests, but rather some test data, a test tar archive, etc., and some functions for working with these.  See the comments therein.

## Demo app

For a demo, run `elm make Main.elm` in `/examples`, then click on the resulting `index.html` file.  

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

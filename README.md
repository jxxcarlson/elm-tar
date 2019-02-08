# Elm-Tar

With this package you can both create and extract tar archives.  To create
a tar archive, use
```
   createArchive List (Metadata, Data) -> Bytes
```
To extract a tar archive. use

```
    extractArchive tarArchive
```

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

For `Hex`, see `jxxcarlson/hex`.  Next, we create an archive consisting of one text file and one binary file:
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


## Demo app

For a demo, run `elm make Main.elm` in `/examples`, then click on the resulting `index.html` file.  

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

# Elm-Tar

With this package you can both create and extract tar archives.  Use
```
   createArchive []
Use

```
    extractArchive tarArchive
```

to extract a tar archive.  The result is a list of elements of type `(MetaData, Data)`.

## Tarring text files

The example below shows how to use the present package with`elm/bytes` and `elm/file` to tar a text file, then download the data as `test.tar`.

```
   import Tar exposing(defaultFileRecord)
   fileRecord =
       { defaultFileRecord | filename = "test123.txt" }

   content =
       "This is a test (ho ho ho).\nIt is a frabjous day!"

   bytes = encodeTextFiles [ ( fileRecord, content ) ] |> Bytes.Encode.encode

   File.Download.bytes "test.tar" "application/x-tar" bytes
```

To archive more files, just put more pairs `(fileRecord, content)`in the list above.



## Tarring arbitrary files

The example below shows how to make an archive for a set of files some of which are binary, some of which are text.

```
  fileRecord_ =
      defaultFileRecord

  fileRecord1 =
      { fileRecord_ | filename = "a.txt" }

  content1 =
      "One two three\n"

  fileRecord2 =
      { fileRecord_ | filename = "b.binary" }

  content2 =
      Hex.toBytes "0123456" |> Maybe.withDefault (encode (Bytes.Encode.unsignedInt8 0))

  Tar.encodeFiles
      [ ( fileRecord1, StringData content1 )
      , ( fileRecord2, BinaryData content2 )
      ]
      |> encode
```

For `Hex`, see `jxxcarlson/hex`.

## Demo app

For a demo, run `elm make Main.elm` in `/examples`, then click on the resulting `index.html` file.  

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

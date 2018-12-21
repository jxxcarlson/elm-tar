# Elm-Tar

With this package you can both create and extract tar archives.


Example 1: Create a tar archive

```
> import Tar exposing(..)

> data1 = ( { defaultFileRecord | filename = "one.txt" }, StringData "One" )
> data2 = ( { defaultFileRecord | filename = "two.txt" }, StringData "Two" )

> myArchive = createArchive [data1, data2]
  <3072 bytes> : Bytes.Bytes
```

Example2: Extract a tar archive
```
    extractArchive myArchive
```

## Data Types

Use the `Data` type to tag strings and
binary data for creating archives.

```
  type Data
      = StringData String
      | BinaryData Bytes
```

When you extract an archive, you create
a value of type `List (FileData, Data)`, where

```
  type alias FileData =
      { fileName : String
      , fileExtension : Maybe String
      , length : Int
      }
```

## Tarring text files

If you want to work closer to the metal, do this:

```
   encodeFiles : List ( FileRecord, Data ) -> Encode.Encoder
```
to tar an arbitrary set of files. The files
may contain either text or binary data, where


```
  type Data
      = StringData String
      | BinaryData Bytes
```
To tar a set of text files, you can use

```
   encodeTextFiles : List (FileRecord, String) -> Encode.Encoder
```
The test app in `examples/Main.elm` illustrates how these are used -- some test data is created, transformed using one of the two functions described above, and then downloaded using the `elm/files` package.  More details are given below.


The example below shows how to use the present package with`elm/bytes` and `elm/file` to tar a text file, then download the data as `test.tar`.

```
   import Tar exposing(defaultFileRecord)
   fileRecord1 =
       { defaultFileRecord | filename = "test123.txt" }

   content1 =
       "This is a test (ho ho ho).\nIt is a frabjous day!"

   bytes = sencodeTextFiles [ ( fileRecord1, content1 ) ] |> Bytes.Encode.encode

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
      Hex.toBytes "616263646566" |> Maybe.withDefault (encode (Bytes.Encode.unsignedInt8 0))

  Tar.encodeFiles
      [ ( fileRecord1, StringData content1 )
      , ( fileRecord2, BinaryData content2 )
      ]
      |> encode
```

For `Hex`, see `jxxcarlson/hex`.

## Demo app

For a demo, run `elm make Main.elm` in `/examples`, then click on the resulting `index.html` file.  To tar text files only, uncomment line 65 and comment out lines 66-71.

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

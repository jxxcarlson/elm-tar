# Elm-Tar

With this package you can create tar archives.  Use

```
   encodeFiles : List ( FileRecord, Data ) -> Encode.Encoder
```
to tar an arbitrary set of files, whch may contain either text or binary
data, where

```
  type Data
      = StringData String
      | BinaryData Bytes
```
To tar a set of text files, you can use

```
   encodeTextFiles : List (FileRecord, String) -> Encode.Encoder
```
The test app in `examples/Main.elm` illustrates how these are used --
some test data is created, transformed using one of the two
functions described above, and then downloaded using the
`elm/files` package.  More details are given below.

## Tarring text files

The example below shows how to use the present package with
`elm/bytes` and `elm/file` tar a text file, then
download the data as `test.tar`.

```
   fileRecord1 =
       { Tar.defaultFileRecord | filename = "test123.txt" }

   content1 =
       "This is a test (ho ho ho).\nIt is a frabjous day!"

   bytes = Tar.encodeTextFiles [ ( fileRecord1, content1 ) ] |> Bytes.Encode.encode

   File.Download.bytes ("test.tar") "application/x-tar" bytes
```

To archive more files, just put more pairs `(fileRecord, content)`in the list above.



## Tarring arbitrary files

The example below shows how to make an archive for a set of files
some of which are binary, some of which are text.

```
  fileRecord_ =
      Tar.defaultFileRecord

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

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

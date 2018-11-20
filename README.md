# Elm-Tar

With this package you can create tar archives using the function

```
   encodeFiles : List (FileRecord, String) -> Encode.Encoder
```

Here is an example which uses `elm/bytes` and `elm/file`:

```
   fileRecord1 =
       { Tar.defaultFileRecord | filename = "test123.txt" }

   content1 =
       "This is a test (ho ho ho).\nIt is a frabjous day!"

   bytes = Tar.encodeFiles [ ( fileRecord1, content1 ) ] |> Bytes.Encode.encode

   File.Download.bytes ("test.tar") "application/x-tar" bytes
```

Just put more pairs `(fileRecord, content)`in the list above to archive more files.

There is a small test app in the `examples` folder of the GitHub repo that shows how this is done.

## References

I've used https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format.

# Elm-Tar

With this package you can create tar archives. Use

```
   encodeFiles : List (FileRecord, String) -> Encode.Encoder

   fileRecord1 =
       { Tar.defaultFileRecord | filename = "test123.txt" }

   content1 =
       "This is a test (ho ho ho).\nIt is a frabjous day!"

   bytes = Tar.encodeFiles [ ( fileRecord1, content1 ) ] |> Bytes.Encode.encode

   Download.bytes ("test.tar") "application/x-tar" bytes
```

Just put more pairs `(fileRecord, content)`in the list above toarchive more files.

There is a small test app in the `examples` folder of the GitHub repo that shows how this is done.

## Bug

__One problem__ in case anyone gives this a try in its current rough state.  When multiple files are in the archive, I can see the files using `Atom`, as well as their contents,  When I extract the files, only the first one is untarred.  No error message is given.  Don’t have a diagnosis yet.

## References

I've used [Wikipedia](https://en.wikipedia.org/wiki/Tar_(computing) as my reference for the tar file format. Is there somethig better?

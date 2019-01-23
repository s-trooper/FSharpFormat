module FSharpFormat.Cmd.FileFinder
open System
open System.IO

let srcExtPattern = "*.fs"
let ignoreDirs = [".git"; "bin"; "obj"; "packages"]

let rec getAllFiles(dir: string, pattern) =
    seq {
        for file1 in Directory.EnumerateFiles(dir, pattern) do
            yield file1

        for subdir in Directory.EnumerateDirectories(dir) do
            let dirname = Path.GetFileName(subdir)
            if not <| dirname.StartsWith(".") &&
               not <| Seq.contains(dirname) ignoreDirs
            then
                for file2 in getAllFiles(subdir, pattern) do
                    yield file2
    }

let getSourceFiles(path, recursive) =
    if String.IsNullOrWhiteSpace(path) then
        []
    elif File.Exists(path) then
        [path]
    elif not <| Directory.Exists(path) then
        []
    elif recursive then
        getAllFiles(path, srcExtPattern)
        |> Seq.toList
    else
        Directory.EnumerateFiles(path, srcExtPattern)
        |> Seq.toList

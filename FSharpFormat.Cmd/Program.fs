module FSharpFormat.Cmd.Program
open FSharpFormat
open FileFinder
open Options
open System
open System.IO

let format = SourceFormatter(Formatters.all).Format

let err(msg: string) =
    Console.ForegroundColor <- ConsoleColor.Red
    Console.Error.WriteLine(msg)
    Console.ResetColor()
    1

let infoBegin(srcFile, files) =
    let fileCount = Seq.length(files)
    let numLen = fileCount.ToString().Length.ToString()
    let fileIdx = files |> Seq.findIndex ((=) srcFile)

    let frm = "({0," + numLen + "} / {1}) File: {2}"
    Console.Write(frm, fileIdx + 1, fileCount, srcFile)

let infoEnd(perf: int64) =
    Console.WriteLine(" ({0} ms)", perf)

let formatFiles(files, opt) =
    let outBuf =
        if opt.StdOut then
            Some(new StreamWriter(Console.OpenStandardOutput()))
        elif not <| String.IsNullOrWhiteSpace(opt.Output) then
            Some(new StreamWriter(opt.Output))
        else
            None

    for srcFile in files do
        let sw = System.Diagnostics.Stopwatch()
        try
            infoBegin(srcFile, files)
            sw.Start()

            let src = File.ReadAllText(srcFile)
            let formattedSrc = format(src)

            match outBuf with
            | None -> File.WriteAllText(srcFile, formattedSrc)
            | Some(buf) -> buf.Write(formattedSrc)

            sw.Stop()
            infoEnd(sw.ElapsedMilliseconds)

        with ex ->
            sw.Stop()
            infoEnd(sw.ElapsedMilliseconds)

            err("error: Cann't format file '" + srcFile + "'\n" + ex.ToString())
            |> ignore

    match outBuf with
    | Some(buf) ->
        buf.Flush()
        buf.Close()
    | None -> ()
    0

[<EntryPoint>]
let main(args) =
    let opts = parseArgs(args)
    match opts with
    | Some opt ->
        let files = getSourceFiles(opt.Input, opt.Recurse)
        if Seq.isEmpty(files) then
            err("error: No source files found in '" + opt.Input + "'")
        else
            formatFiles(files, opt)
    | None -> 2

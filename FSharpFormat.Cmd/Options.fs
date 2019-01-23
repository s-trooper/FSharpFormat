module FSharpFormat.Cmd.Options
open System

[<Literal>]
let usage = """
Usage: fsfmt [OPTION] SOURCE
Formats the SOURCE. If SOURCE is a folder then formats all .fs files within it.
If there is no defined OUTPUT then writes the formatted source code back to the SOURCE.

Options:
  -r, --recurse     Processes the input folder recursively
      --stdout      Writes the formatted source code to the standard output
  -o, --out <FILE>  Writes the formatted source code into <FILE>
  -h, --help        Displays this information
"""
let trimEOL(x: string) = x.Trim('\r').Trim('\n')

type Options = {
    Recurse: bool
    StdOut: bool
    Input: string
    Output: string
}

let parseArgs(args) =
    let rec parse (xs) =
        match xs with
        | [] -> { Recurse = false; StdOut = false; Input = ""; Output = "" }
        | "-r" :: rs
        | "--recurse" :: rs -> { parse(rs) with Recurse = true }
        | "--stdout" :: rs -> { parse(rs) with StdOut = true }
        | "-o" :: dest :: rs
        | "--out" :: dest :: rs when not <| dest.StartsWith("-") -> { parse(rs) with Output = dest }
        | p :: rs when p.StartsWith("-") -> parse(rs)
        | src :: rs -> { parse(rs) with Input = src }

    if Seq.isEmpty(args) || args |> Seq.contains("-h") || args |> Seq.contains("--help") then
        Console.WriteLine(usage |> trimEOL)
        None
    else
        Some <| parse(Seq.toList(args))

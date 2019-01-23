module FSharpFormat.Program
open System
open NUnitLite

[<EntryPoint>]
[<STAThread>]
let main args =
    let runner = new AutoRun()
    runner.Execute(Array.append args [|"--noheader"|])

module FSharpFormat.TokenizerTests
open NUnit.Framework
open FSharpFormat
open System.IO

let format = SourceFormatter(Formatters.all).Format

[<Test>]
let ``handle compiler directives``() =
    let arranged = """
let x = @"
#if D1
#endif
    ""\n""
"
"""
    let expected = """
let x = @"
#if D1
#endif
    ""\n""
"
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
#if D1
#else // C
    1
#endif
"""
    let expected = """
#if D1
#else // C
    1
#endif
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
let f =    
#if  D2  
        1
#endif // C
        3
"""
    let expected = """
let f =
#if D2
        1
#endif // C
        3
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
#if !D
        let f =
#if E
            if x then 
                1
            else 
#endif
                2
        let y = 3
#endif
"""
    let expected = """
#if !D
        let f =
#if E
            if x then
                1
            else
#endif
                2
        let y = 3
#endif
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
let f =    
#if   FX_NO_DEFAULT_ENCODING  
        1  
#else
        3  
#endif
    x = 5  
"""
    let expected = """
let f =
#if FX_NO_DEFAULT_ENCODING
        1
#else
        3
#endif
    x = 5
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``multi token operators``() =
    let arranged = """
    a  [|f<'T>|]
    1
"""
    let expected = """
    a [|f<'T>|]
    1
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = ".&."
    let expected = ".&."
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """let (>=.)"""
    let expected = """let (>=.)"""

    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``keep spaces in directives``() =
    let arranged = "#"
    let expected = "#"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "#nowarn \"1182\""
    let expected = "#nowarn \"1182\""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """# 3 "..\..\absil\ilpars.fsy"""
    let expected = """# 3 "..\..\absil\ilpars.fsy"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """(# 3 #)"""
    let expected = """(# 3 #)"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

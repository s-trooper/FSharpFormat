module FSharpFormat.SpacingTests
open NUnit.Framework
open FSharpFormat

let format = SourceFormatter(Formatters.all).Format

[<Test>]
let ``empty source``() =
    let arranged = ""
    let expected = ""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``keep original EOL``() =
    let arranged = "\rMAC\nNIX\r\nWIN"
    let expected = "\rMAC\nNIX\r\nWIN"B
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``trim trailing spaces``() =
    let arranged = "
let x = 1  
    \t
"
    let expected = "
let x = 1

"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
   """
    let expected = """
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))
    let arranged = """
            
   """
    let expected = """

"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``keep trailing spaces in strings``() =
    let arranged = "
let x = \"  
a  
   \t
\"
"
    let expected = "
let x = \"  
a  
   \t
\"
"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``keep trailing spaces in comments``() =
    let arranged = "
    //   
      
(* 
  \t
*)   "
    let expected = "
    //   

(* 
  \t
*)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``operators``() =
    let arranged = "if a&&b then"
    let expected = "if a && b then"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "if a  >  b then"
    let expected = "if a > b then"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x=1    "
    let expected = "let x = 1"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x  =   1 "
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x()=1    "
    let expected = "let x() = 1"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x=1+2  "
    let expected = "let x = 1 + 2"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``infix operators``() =
    let arranged = """
let +AF8-
    S +ACJvIgAi-
"""
    let expected = """
let +AF8-
    S +ACJvIgAi-
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "f() && g()"
    let expected = "f() && g()"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "x.[y-1..]"
    let expected = "x.[y - 1..]"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "1 .&. 2"
    let expected = "1 .&. 2"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "a-b"
    let expected = "a - b"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "1   :: [1]"
    let expected = "1 :: [1]"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "'a'::rs"
    let expected = "'a' :: rs"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``prefix operators``() =
    let arranged = """
    && float32"""
    let expected = """
    && float32"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "| -1 -> None"
    let expected = "| -1 -> None"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    -1 // C"""
    let expected = """
    -1 // C"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x=C(-n)"
    let expected = "let x = C(-n)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let f(a & b) = 1"
    let expected = "let f(a & b) = 1"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "f a &b"
    let expected = "f a &b"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "Some -1"
    let expected = "Some -1"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "f a &b"
    let expected = "f a &b"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "x := !x+1"
    let expected = "x := !x + 1"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x=[-1; 2]"
    let expected = "let x = [-1; 2]"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``point/invokation operator``() =
    let arranged = "a   . b()"
    let expected = "a.b()"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``tuples``() =
    let arranged = "( !>>> )"
    let expected = "( !>>> )"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "( *. )"
    let expected = "( *. )"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "( 1 , 2 )"
    let expected = "(1, 2)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "( 1,2 )"
    let expected = "(1, 2)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "(1 + 2)"
    let expected = "(1 + 2)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "T( true )"
    let expected = "T(true)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "let x=f(a,b)"
    let expected = "let x = f(a, b)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``function definition``() =
    let arranged = "let     x (a : string):int =  1"
    let expected = "let x (a: string): int = 1"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``lists``() =
    let arranged = """let x = ["1";a] """
    let expected = """let x = ["1"; a]"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """let x  =  [1  ;   a] """
    let expected = """let x = [1; a]"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``records``() =
    let arranged = """
    let x =    
        { X="A"   
          Y   =   ["B"     ; "C" ; "D"]   }
"""
    let expected = """
    let x =
        { X = "A"
          Y = ["B"; "C"; "D"] }
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``unions``() =
    let arranged = """
type T = 
    |   A 
    |B
"""
    let expected = """
type T =
    | A
    | B
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``keywords``() =
    let arranged = "a->b"
    let expected = "a -> b"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``overlapped operators``() =
    let arranged = """
f>>>f // could be part of generics
f>?f
f&&&f
f++f
"""
    let expected = """
f>>>f // could be part of generics
f >? f
f &&& f
f ++ f
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``generics 0``() =
    let arranged = "X<'T> : 'T"
    let expected = "X<'T> : 'T"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``generics``() =
    let arranged = "typedefof<T<_>>, typedefof<A<_>>"
    let expected = "typedefof<T<_>>, typedefof<A<_>>"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "X<'T> : 'T"
    let expected = "X<'T> : 'T"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "[| A<B<'T>> |]"
    let expected = "[| A<B<'T>> |]"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "delegate<'Args, unit>"
    let expected = "delegate<'Args, unit>"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "type X<'T> = IEvent<Handler<'T>, 'T>"
    let expected = "type X<'T> = IEvent<Handler<'T>, 'T>"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "T<[<M>] 'M>"
    let expected = "T<[<M>] 'M>"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "X<_>"
    let expected = "X<_>"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "X<int>()"
    let expected = "X<int>()"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "type X<'T,'V>"
    let expected = "type X<'T, 'V>"
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = "(X<T>)"
    let expected = "(X<T>)"
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``active pattern``() =
    let arranged = "let (|X|Y|_|)  =  x | y"
    let expected = "let (|X|Y|_|) = x | y"

    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``pattern match``() =
    let arranged = """
    match x with 
    |A ->   B
    | _-> ()
"""
    let expected = """
    match x with
    | A -> B
    | _-> ()
"""

    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``extra cases``() =
    let arranged = "let x = {| X = 1 |}"
    let expected = "let x = {| X = 1 |}"

    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """  namespace Neg41a 
  type X = A | B
"""
    let expected = """  namespace Neg41a
  type X = A | B
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

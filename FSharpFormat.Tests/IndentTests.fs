module FSharpFormat.IndentTests
open NUnit.Framework
open FSharpFormat

let format = SourceFormatter(Formatters.all).Format

[<Test>]
let ``copy and update record expression``() =
    let arranged = """
let opts   = { opts  with A = true  
                          B = false // seq off, {}  
                          C = { opts  with A2 = 1
                                           B2 = 2 }
                          D = opts.X / Y } 
    1
"""
    let expected = """
let opts = { opts with A = true
                       B = false // seq off, {}  
                       C = { opts with A2 = 1
                                       B2 = 2 }
                       D = opts.X / Y }
    1
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``let expression``() =
    let arranged = """
    match l with 
    | a,x::y -> let z = 1
                x::z
    | z -> 2
"""
    let expected = """
    match l with
    | a, x :: y -> let z = 1
                   x :: z
    | z -> 2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    M (fun ctxt -> 
        match x with 
        | None -> 
            let y = 
                7
            if y then 
                1
            else 
                2
        | Some _ -> 
            3
"""
    let expected = """
    M (fun ctxt ->
        match x with
        | None ->
            let y =
                7
            if y then
                1
            else
                2
        | Some _ ->
            3
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
let f =
    g arg1
        "123" 
"""
    let expected = """
let f =
    g arg1
        "123"
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
->
    let rec check i =
        if i < 5 then 0 else
            let c = 2
            if c <> 0 then c else check (i - 1)
    check 6
"""
    let expected = """
->
    let rec check i =
        if i < 5 then 0 else
            let c = 2
            if c <> 0 then c else check (i - 1)
    check 6
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``object expression``() =
    let arranged = """
    new () = { }      
    abstract A : 'a with get,set
    abstract M : unit -> 'a 
"""
    let expected = """
    new () = { }
    abstract A: 'a with get, set
    abstract M: unit -> 'a
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
        X { new AstTraversal.AstVisitorBase<_>() with
        member this.M =
            let expr = 1 }
"""
    let expected = """
        X { new AstTraversal.AstVisitorBase<_>() with
        member this.M =
            let expr = 1 }
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    {  new E<_>() with
            member this.M x =
                if n1 && n2 then
                    x <- f.Invoke(e1, e2)
                    true
                else
                    false
            member this.D() =
                try
                    e1.Dispose()
                finally
                    e2.Dispose()
    }
"""
    let expected = """
    { new E<_>() with
            member this.M x =
                if n1 && n2 then
                    x <- f.Invoke(e1, e2)
                    true
                else
                    false
            member this.D() =
                try
                    e1.Dispose()
                finally
                    e2.Dispose()
    }
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let x =
        (f (fun () ->
            { new T<_> with
                    member x.C1 = 1
                interface I with
                    member x.D() = () }))
    let y = 2
"""
    let expected = """
    let x =
        (f (fun () ->
            { new T<_> with
                    member x.C1 = 1
                interface I with
                    member x.D() = () }))
    let y = 2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
let x =
    let c = { new IComparer<'T> with member __.Compare(x, y) = 1 }
    2
"""
    let expected = """
let x =
    let c = { new IComparer<'T> with member __.Compare(x, y) = 1 }
    2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``arrow expression``() =
    let arranged = """
    lock x (fun _ ->
        begin match !y with
        | Some (Some e) -> 1
        | _ -> ()
        end
        z)
"""
    let expected = """
    lock x (fun _ ->
        begin match !y with
        | Some (Some e) -> 1
        | _ -> ()
        end
        z)
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
      let x r =
          lock r (fun () -> match !r with None -> None | Some _ as res -> r := None; res)

      let y = 1
"""
    let expected = """
      let x r =
          lock r (fun () -> match !r with None -> None | Some _ as res -> r := None; res)

      let y = 1
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
let x  =  fun  y  -> 
        1
        2
    3
"""
    let expected = """
let x = fun y ->
        1
        2
    3
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``match expression``() =
    let arranged = """
    DepthCheck ndeep m  ++ (fun () -> 
    (fun () ->     
    match X with 
      | A
          when (// The input type. 
                (IsNonDecimalNumericOrIntegralEnumType g argty || isStringTy g argty || isCharTy g argty) &&
                // The output type
                (IsNonDecimalNumericOrIntegralEnumType g rty || isCharTy g rty) &&
                // Exclusion: IntPtr and UIntPtr do not support .Parse() from string 
                not (isStringTy g argty && isNativeIntegerTy g rty) &&
                // Exclusion: No conversion from char to decimal
                not (isCharTy g argty && isDecimalTy g rty)) ->

          1

      // C1
      | B
          when (// The input type.2 
                (IsNumericOrIntegralEnumType g argty || isStringTy g argty) &&
                // The output type
                (isDecimalTy g rty)) -> 

          2))
"""
    let expected = """
    DepthCheck ndeep m ++ (fun () ->
    (fun () ->
    match X with
      | A
          when (// The input type. 
                (IsNonDecimalNumericOrIntegralEnumType g argty || isStringTy g argty || isCharTy g argty) &&
                // The output type
                (IsNonDecimalNumericOrIntegralEnumType g rty || isCharTy g rty) &&
                // Exclusion: IntPtr and UIntPtr do not support .Parse() from string 
                not (isStringTy g argty && isNativeIntegerTy g rty) &&
                // Exclusion: No conversion from char to decimal
                not (isCharTy g argty && isDecimalTy g rty)) ->

          1

      // C1
      | B
          when (// The input type.2 
                (IsNumericOrIntegralEnumType g argty || isStringTy g argty) &&
                // The output type
                (isDecimalTy g rty)) ->

          2))
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    match xs,ys with
    | x::xs,y::ys -> let cxy = 1
                     if cxy=0 then 2 else 3 
"""
    let expected = """
    match xs, ys with
    | x :: xs, y :: ys -> let cxy = 1
                          if cxy = 0 then 2 else 3
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    match xs with
    | ((  321
         | 4))->(match x with
                 | _ - > b)
    |[(1 
     | 2)] -> a
"""
    let expected = """
    match xs with
    | ((321
         | 4)) -> (match x with
                   | _ - > b)
    | [(1
     | 2)] -> a
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    match x with
    | _ ->
        Seq.filter (fun v ->
            v.CompiledName = vName &&
                match y with
                | p -> 1
                | _ -> 2
        )
"""
    let expected = """
    match x with
    | _ ->
        Seq.filter (fun v ->
            v.CompiledName = vName &&
                match y with
                | p -> 1
                | _ -> 2
        )
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
            let blockLet = match ctxt with | CtxtSeqBlock _ -> true 
                                           | CtxtMatchClauses _ -> true 
                                           | _ -> false
"""
    let expected = """
            let blockLet = match ctxt with | CtxtSeqBlock _ -> true
                                           | CtxtMatchClauses _ -> true
                                           | _ -> false
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let x = (function
    | 1 -> 2)
"""
    let expected = """
    let x = (function
    | 1 -> 2)
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
let path = 
    [|   match  x  with 
         | 1 ->  match  y  with 
                 | 2 -> ()
         | _ -> yield! x.Split([|'.'|]) |]
    1
"""
    let expected = """
let path =
    [| match x with
       | 1 -> match y with
              | 2 -> ()
       | _ -> yield! x.Split([|'.'|]) |]
    1
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``if else expression``() =
    let arranged = """
if x then invalidOpFmt "a"
           [|1|]
"""
    let expected = """
if x then invalidOpFmt "a"
           [|1|]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    if x then 
    // C1
      1
    else // C o
      2
"""
    let expected = """
    if x then
    // C1
      1
    else // C o
      2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    if a
    then
        3
"""
    let expected = """
    if a
    then
        3
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
        [ if a || 
             b then
           3 ]
"""
    let expected = """
        [ if a ||
             b then
           3 ]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
->  if  x  then 
    
        1
          3
    else
      2
"""
    let expected = """
-> if x then

        1
          3
   else
      2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
if  x  
then   1
2
"""
    let expected = """
if x
then 1
2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``do expression``() =
    let arranged = """
->  do ignore
    let x = 1
"""
    let expected = """
-> do ignore
   let x = 1
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
do()

type T() =
    inherit P()  
"""
    let expected = """
do ()

type T() =
    inherit P()
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
->
    for x in xs do
        1
    2
"""
    let expected = """
->
    for x in xs do
        1
    2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
while x do
    while y do
        1
    2
"""
    let expected = """
while x do
    while y do
        1
    2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``list expression``() =
    let arranged = """
    [|  1
        (3) |]
"""
    let expected = """
    [| 1
       (3) |]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    P(L              [  a 1
                        b 2
                        c 3])
"""
    let expected = """
    P(L [ a 1
          b 2
          c 3])
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    [ mkILNonGenericInstanceMethod
         (a,
         mkMethodBody(true, [], 2, b 
                   (c))
      |> addMethodGeneratedAttrs ]
"""
    let expected = """
    [ mkILNonGenericInstanceMethod
         (a,
         mkMethodBody(true, [], 2, b
                   (c))
      |> addMethodGeneratedAttrs ]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let f1 =
      [
# 1 ""
        "array"
        "bigint"
      ] |> Set.ofList
    let f2 = 1
"""
    let expected = """
    let f1 =
      [
# 1 ""
        "array"
        "bigint"
      ] |> Set.ofList
    let f2 = 1
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    [ for x in xs do 
            yield 
                longBlaBla
                   (1,
                    2)
                |> y ]
"""
    let expected = """
    [ for x in xs do
            yield
                longBlaBla
                   (1,
                    2)
                |> y ]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    [|   match   x  with
         | y   -> 0
         yield! a |]
"""
    let expected = """
    [| match x with
       | y -> 0
       yield! a |]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let x=[ 1
            2
            3 ]
"""
    let expected = """
    let x = [ 1
              2
              3 ]
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``seq expression``() =
    let arranged = """
    (fun x ->
        Some { A = 1
               B = 2 })
"""
    let expected = """
    (fun x ->
        Some { A = 1
               B = 2 })
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

[<Test>]
let ``tuple expression``() =
    let arranged = """
        let result = Set.fold 
                            (1) 
                            0 
                            input
"""
    let expected = """
        let result = Set.fold
                            (1)
                            0
                            input
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let x=S(0, 
           (1), 
           (2))
"""
    let expected = """
    let x = S(0,
              (1),
              (2))
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    (a
        let x =
            b
                // 
        z)
"""
    let expected = """
    (a
        let x =
            b
                // 
        z)
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
        (fun _ -> 
            let x = 
                accFreeInTypes a (P(amap, m)) 
                       (freeInType b (B(amap, m)))
            let free = 1
            free.IsEmpty)
"""
    let expected = """
        (fun _ ->
            let x =
                accFreeInTypes a (P(amap, m))
                       (freeInType b (B(amap, m)))
            let free = 1
            free.IsEmpty)
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let x = 
        (holes |> Array.fold (fun acc holeType -> 
            acc  +   match holeType with 
                    | _ -> 0))
    let errPrefix=match optErrNum with
                   | Some n -> 1
    2
"""
    let expected = """
    let x =
        (holes |> Array.fold (fun acc holeType ->
            acc + match holeType with
                  | _ -> 0))
    let errPrefix = match optErrNum with
                    | Some n -> 1
    2
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    (fun ctxt -> 
        lock f (fun () -> 
            if x = 0 then 
                x <- Some(fun res -> Q(res))
                1
            else
                2)
        3)
"""
    let expected = """
    (fun ctxt ->
        lock f (fun () ->
            if x = 0 then
                x <- Some(fun res -> Q(res))
                1
            else
                2)
        3)
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    let x a1 a2 =
        async {
            let start a f =
                Async.Start(a, 
                    (1),
                    (b)
                    )
            2
        }

    let y =
        3
"""
    let expected = """
    let x a1 a2 =
        async {
            let start a f =
                Async.Start(a,
                    (1),
                    (b)
                    )
            2
        }

    let y =
        3
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

    let arranged = """
    type T =
        new() = new T(true)
      
    type V() =
        inherit A()
"""
    let expected = """
    type T =
        new() = new T(true)

    type V() =
        inherit A()
"""
    Assert.That(arranged |> format, Is.EqualTo(expected))

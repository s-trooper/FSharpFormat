namespace rec FSharpFormat
open System.Collections.Generic

type TokenKind =
    | Default
    | Text
    | Keyword
    | Identifier
    | Literal
    | Operator
    | Delimiter
    | WhiteSpace
    | Comment
    | EOL
    | Empty

type Token = {
    LeftColumn: int
    Text: string
    Kind: TokenKind
}
with
    static member Empty = {Text = ""; Kind = TokenKind.Empty; LeftColumn = -1}

type TokenChain = TokenChain of (Token list * Token * Token list) with
    interface IEnumerable<Token> with
        member this.GetEnumerator(): IEnumerator<Token> =
            let ls = seq {
                if this <> TokenChain ([], Token.Empty, []) then
                    let mutable latest = this
                    yield Loc.current(latest)

                    while Loc.isNext(latest) do
                        latest <- Loc.next(latest)
                        yield Loc.current(latest)
            }
            ls.GetEnumerator()

        member this.GetEnumerator(): System.Collections.IEnumerator =
            (this :> IEnumerable<Token>).GetEnumerator() :> System.Collections.IEnumerator

type Loc =
    | RIGHT
    | LEFT
    | SELF

module Loc =
    let create (xs) =
        match xs with
        | [] -> TokenChain ([], Token.Empty, [])
        | h :: t -> TokenChain ([], h, t)

    let isNext (TokenChain(_, _, r)) = not <| Seq.isEmpty(r)
    let isPrev (TokenChain(l, _, _)) = not <| Seq.isEmpty(l)

    // head is rev for performant right op
    let next (z) =
        match z with
        | TokenChain(l, e, []) -> TokenChain(l, e, [])
        | TokenChain(l, e, h :: rt) -> TokenChain(e :: l, h, rt)

    let prev (z) =
        match z with
        | TokenChain([], e, r) -> TokenChain([], e, r)
        | TokenChain(h :: lt, e, r) -> TokenChain(lt, h, e :: r)

    let rec moveToBegin (z) =
        if isPrev(z) then
            prev(z) |> moveToBegin
        else
            z

    let insertAfter (x, z) =
        match z with
        | TokenChain(l, e, []) -> TokenChain(l, e, [x])
        | TokenChain(l, e, r) -> TokenChain(l, e, x :: r)

    let insertBefore (x, z) =
        match z with
        | TokenChain([], e, r) -> TokenChain([x], e, r)
        | TokenChain(l, e, r) -> TokenChain(x :: l, e, r)

    let update (x, z) =
        let (TokenChain(l, _, r)) = z in TokenChain(l, x, r)

    let removeAfter(z) =
        match z with
        | TokenChain([], _, []) -> TokenChain([], Token.Empty, [])
        | TokenChain([], _, r :: rt) -> TokenChain([], r, rt)
        | TokenChain(l :: lt, _, r) -> TokenChain(lt, l, r)

    let removeBefore(z) =
        match z with
        | TokenChain([], _, []) -> TokenChain([], Token.Empty, [])
        | TokenChain(l :: lt, _, []) -> TokenChain(lt, l, [])
        | TokenChain(l, _, r :: rt) -> TokenChain(l, r, rt)

    let current (TokenChain(_, e, _)) = e
    let toList (z) =
        let (TokenChain(l, e, r)) = moveToBegin(z) in
            l @ e :: r

    let length (TokenChain(l, e, r)) =
        List.length(l) + 1 + List.length(r)

    let rec apply(format, loc) =
        let nextLoc = format(loc)
        if isNext(nextLoc) then
            apply(format, next(nextLoc))
        else
            nextLoc

    let rec applyWith(format, loc, state) =
        let nextLoc, state = format(loc, state)
        if isNext(nextLoc) then
            applyWith(format, next(nextLoc), state)
        else
            nextLoc

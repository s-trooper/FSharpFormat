[<AutoOpen>]
module private FSharpFormat.FormatterHelper

open System.Collections.Generic

let SpaceTok = {Text = " "; Kind = TokenKind.WhiteSpace; LeftColumn = -1}

let (|Space|_|) (x) =
    match x with
    | {Kind = TokenKind.WhiteSpace} -> Some Space
    | _ -> None

let (|Newline|_|) (x) =
    match x with
    | {Kind = TokenKind.EOL} -> Some Newline
    | _ -> None

let (|PipeOperator|_|) (x) =
    match x with
    | {Text = "|"; Kind = TokenKind.Delimiter} -> Some PipeOperator
    | _ -> None

let (|Operator|_|) (x) =
    match x with
    | {Kind = TokenKind.Operator} -> Some Operator
    | _ -> None

let (|PointOperator|_|) (x) =
    match x with
    | {Text = "."; Kind = TokenKind.Delimiter} -> Some PointOperator
    | _ -> None

let (|AngleBracketOperator|_|) (x) =
    let isTrue(xs) =
        Seq.forall (fun x -> x = '<') xs ||
        Seq.forall (fun x -> x = '>') xs

    match x with
    | {Text = s; Kind = TokenKind.Operator} when isTrue(s.ToCharArray()) -> Some AngleBracketOperator
    | _ -> None

let (|Delimiter|_|) (x) =
    match x with
    | {Text = s; Kind = TokenKind.Delimiter} when s = ";" || s = "," || s = ":" -> Some Delimiter
    | _ -> None

let (|Keyword|_|) (x) =
    match x with
    | {Text = _; Kind = TokenKind.Keyword} -> Some Keyword
    | _ -> None

let (|TupleOpen|_|) (x) =
    match x with
    | {Text = "("; Kind = TokenKind.Delimiter} -> Some TupleOpen
    | _ -> None

let (|TupleClose|_|) (x) =
    match x with
    | {Text = ")"; Kind = TokenKind.Delimiter} -> Some TupleClose
    | _ -> None

let (|SeqOpen|_|) (x) =
    match x with
    | {Text = "begin"; Kind = TokenKind.Keyword}
    | {Text = "{"; Kind = TokenKind.Delimiter}
    | {Text = "[|"; Kind = TokenKind.Delimiter}
    | {Text = "["; Kind = TokenKind.Delimiter} -> Some SeqOpen
    | _ -> None

let (|SeqClose|_|) (x) =
    match x with
    | {Text = "end"; Kind = TokenKind.Keyword}
    | {Text = "}"; Kind = TokenKind.Delimiter}
    | {Text = "|]"; Kind = TokenKind.Delimiter}
    | {Text = "]"; Kind = TokenKind.Delimiter} -> Some SeqClose
    | _ -> None

let (|Ident|_|) (x) =
    match x with
    | {Text = _; Kind = TokenKind.Identifier} -> Some Ident
    | _ -> None

let (|SpaceOrComment|_|) (x) =
    match x with
    | Space
    | {Kind = TokenKind.Comment} -> Some SpaceOrComment
    | _ -> None

let toPred(pattern) =
    let pred(loc) =
        let tok = Loc.current(loc)
        Option.isSome(pattern(tok))

    pred

let isNext(dir, loc) =
    match dir with
    | Loc.RIGHT -> Loc.isNext(loc)
    | Loc.LEFT -> Loc.isPrev(loc)
    | Loc.SELF -> false

let next(dir, loc) =
    let nloc =
        match dir with
        | Loc.RIGHT -> Loc.next(loc)
        | Loc.LEFT -> Loc.prev(loc)
        | Loc.SELF -> loc

    Loc.current(nloc), nloc

let rec findEOL(dir, loc) =
    let tok, nloc = next(dir, loc)
    match tok with
    | _ when not <| isNext(dir, nloc) -> nloc
    | Newline -> nloc
    | _ -> findEOL(dir, nloc)

let rec findNotEmpty(dir, loc) =
    let tok, nloc = next(dir, loc)
    match tok with
    | _ when not <| isNext(dir, nloc) -> nloc
    | Newline
    | SpaceOrComment -> findNotEmpty (dir, nloc)
    | {Text = txt; Kind = TokenKind.Delimiter}
    | {Text = txt; Kind = TokenKind.Keyword} when txt.StartsWith("#") ->
        let eol = findEOL(Loc.RIGHT, nloc)
        findNotEmpty(dir, eol)
    | _ -> nloc

let rec isTokenWith(predicate, breakPred, dir, loc, maxCount) =
    let rec isTokenRec(predicate, breakPred, dir, loc2, wordCount) =
        let _, nloc = next(dir, loc2)
        if wordCount > maxCount || not <| isNext(dir, nloc) then
            false
        else if breakPred(wordCount, nloc) then
            false
        else if predicate(nloc) then
            true
        else
            let isWord = not <| toPred(|Space|_|)(nloc)
            let count = if isWord then wordCount + 1 else wordCount

            isTokenRec(predicate, breakPred, dir, nloc, count)

    isTokenRec(predicate, breakPred, dir, loc, 0)

let isToken(predicate, breakPred, dir, loc) =
    let MaxWordCount = 16
    isTokenWith(predicate, breakPred, dir, loc, MaxWordCount)

let rec isNextToken(predicate, dir, loc) =
    let tok, nloc = next(dir, loc)
    match tok with
    | _ when predicate(nloc) -> true
    | _ when not <| isNext(dir, nloc) -> false
    | Space -> isNextToken(predicate, dir, nloc)
    | _ -> false

let isSpace(dir, loc) =
    isNextToken(toPred(|Space|_|), dir, loc)

let isComment(dir, loc) =
    let isCommentTok(loc) =
        let tok = Loc.current(loc)
        match tok with
        | {Kind = TokenKind.Comment} -> true
        | _ -> false

    isNextToken(isCommentTok, dir, loc)

let isEOL(dir, loc) =
    let isNewLine(nloc) =
        let isLast =
            if dir = Loc.SELF then
                false
            else
                not <| isNext(dir, loc)

        toPred(|Newline|_|)(nloc) || isLast

    isNextToken(isNewLine, dir, loc)

let rec removeSpaceBefore2 (changed) (loc) =
    if isNextToken(toPred(|Space|_|), Loc.LEFT, loc) &&
       not <| isEOL(Loc.LEFT, Loc.prev(loc)) then
        Loc.removeBefore(Loc.prev(loc))
        |> removeSpaceBefore2(true)
    else
        (loc, changed)

let removeSpaceBefore (loc) =
    let loc, _ = removeSpaceBefore2(false) loc
    loc

let rec removeSpaceAfter2 (changed) (loc) =
    if isNextToken(toPred(|Space|_|), Loc.RIGHT, loc) then
        Loc.removeAfter(Loc.next(loc))
        |> removeSpaceAfter2(true)
    else
        (loc, changed)

let removeSpaceAfter (loc) =
    let loc, _ = removeSpaceAfter2(false) loc
    loc

let insertSpaceAfter (loc) =
    if isEOL(Loc.RIGHT, loc) then loc
    else Loc.insertAfter(SpaceTok, loc)

let insertSpaceBefore (loc) =
    if isEOL(Loc.LEFT, loc) then loc
    else Loc.insertBefore(SpaceTok, loc)

let removeSpaceAllButOne(dir)(loc) =
    let loc', changed =
        match dir with
        | Loc.LEFT -> loc |> removeSpaceBefore2(false)
        | Loc.RIGHT -> loc |> removeSpaceAfter2(false)
        | Loc.SELF -> loc, false

    if changed then
        match dir with
        | Loc.LEFT -> loc' |> insertSpaceBefore
        | Loc.RIGHT -> loc' |> insertSpaceAfter
        | Loc.SELF -> loc'
    else
        loc

let rec getIndentSize(loc) =
    if Loc.isPrev(loc) then
        let ploc = Loc.prev(loc)
        let t = Loc.current(ploc)
        match t with
        | Newline -> 0
        | _ -> getIndentSize(ploc) + t.Text.Length
    else 0

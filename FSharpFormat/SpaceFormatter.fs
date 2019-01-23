[<AutoOpen>]
module private FSharpFormat.SpaceFormatter

let (|TrailingSpace|_|) (loc) =
    if isEOL(Loc.RIGHT, loc) then
        Some TrailingSpace
    else
        None

let formatPointOp(loc) =
    loc
    |> removeSpaceBefore
    |> removeSpaceAfter

let formatSeq(loc) =
    loc
    |> removeSpaceAllButOne(Loc.LEFT)
    |> removeSpaceAllButOne(Loc.RIGHT)

let formatPipe(loc) =
    let isPat(loc) =
        let isMatch(nloc) =
            let t = Loc.current(nloc)
            match t with
            | SeqOpen
            | Ident -> true
            | _ -> false

        isNextToken(isMatch, Loc.RIGHT, loc)

    let insertIfAllowed(loc) =
        if isSpace(Loc.LEFT, loc) && not <|isSpace(Loc.RIGHT, loc) && isPat(loc) then
            loc
            |> insertSpaceAfter
        else
            loc

    loc
    |> removeSpaceAllButOne(Loc.LEFT)
    |> removeSpaceAllButOne(Loc.RIGHT)
    |> insertIfAllowed

let formatOp(loc) =
    let insertIfAllowed(loc) =
        let isSeq = isNextToken(toPred(|TupleOpen|_|), Loc.LEFT, loc) ||
                    isNextToken(toPred(|SeqOpen|_|), Loc.LEFT, loc)

        if not <| isSpace(Loc.LEFT, loc) &&
           not <| isSpace(Loc.RIGHT, loc) &&
           not <| isEOL(Loc.RIGHT, loc) &&
           not <| isSeq &&
           not <| isNextToken(toPred(|AngleBracketOperator|_|), Loc.RIGHT, loc)
        then
            loc
            |> insertSpaceBefore
            |> insertSpaceAfter
        else
            loc

    loc
    |> removeSpaceAllButOne(Loc.LEFT)
    |> removeSpaceAllButOne(Loc.RIGHT)
    |> insertIfAllowed

let formatDelimeter(loc) =
    let removeSpaceBeforeIfAllowed(loc) =
        let isGenDef = isNextToken(toPred(|AngleBracketOperator|_|), Loc.LEFT, loc)
        match Loc.current(loc) with
        | {Text = ":"; Kind = TokenKind.Delimiter} when isGenDef ->
            removeSpaceAllButOne Loc.LEFT loc
        | _ ->
            removeSpaceBefore(loc)

    loc
    |> removeSpaceBeforeIfAllowed
    |> removeSpaceAfter
    |> insertSpaceAfter

let formatTupleOpen(loc) =
    if not <| isNextToken(toPred(|Operator|_|), Loc.RIGHT, loc) then
        removeSpaceAfter(loc)
    else
        loc

let formatTupleClose(loc) =
    if not <| isNextToken(toPred(|Operator|_|), Loc.LEFT, loc) then
        removeSpaceBefore(loc)
    else
        loc

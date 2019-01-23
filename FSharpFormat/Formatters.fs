module FSharpFormat.Formatters

let trailingSpaceCleaner(tokens: TokenChain) =
    let tokenFormat(loc) =
        match loc with
        | TrailingSpace -> removeSpaceAfter(loc)
        | _ -> loc

    Loc.apply(tokenFormat, tokens)

let spaceFormatter(tokens: TokenChain) =
    let tokenFormat(loc) =
        let tok = Loc.current(loc)
        match tok with
        | PointOperator -> SpaceFormatter.formatPointOp(loc)
        | SeqOpen
        | SeqClose
        | AngleBracketOperator -> SpaceFormatter.formatSeq(loc)
        | PipeOperator -> SpaceFormatter.formatPipe(loc)
        | Keyword
        | Operator -> SpaceFormatter.formatOp(loc)
        | Delimiter -> SpaceFormatter.formatDelimeter(loc)
        | TupleOpen -> SpaceFormatter.formatTupleOpen(loc)
        | TupleClose -> SpaceFormatter.formatTupleClose(loc)
        | _ -> loc

    Loc.apply(tokenFormat, tokens)

let indentFormatter(tokens: TokenChain) =
    let rules = [(isObjectExpressionBegin, isObjectExpressionEnd, ObjExp)
                 (isTupleScopeBegin, isTupleScopeEnd, Tuple)
                 (isSeqScopeBegin, isSeqScopeEnd, Seq)
                 (isIfScopeBegin, isIfScopeEnd, IfThen)
                 (isMatchScopeBegin, isMatchScopeEnd, Match)
                 (isDoBlockScopeBegin, isDoBlockScopeEnd, DoBlock)
                 (isBlockScopeBegin, isBlockScopeEnd, Block)]

    Loc.applyWith(IndentFormatter.formatWith(rules), tokens, [])

let all = [trailingSpaceCleaner; spaceFormatter; indentFormatter]

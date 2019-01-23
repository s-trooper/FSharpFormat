[<AutoOpen>]
module private FSharpFormat.IndentRules

let [<Literal>] MaxShiftLength = 6

type IndentRuleType =
    | Seq
    | Tuple
    | Match
    | DoBlock
    | Block
    | IfThen
    | ObjExp
    | Default

type IndentState = {
    RuleType: IndentRuleType
    NewSize: int
    OldSize: int
    IsShifted: bool
}
type BeginRule = TokenChain -> bool
type EndRule = IndentState -> TokenChain -> bool

let isWithinThreshold(state, loc) =
    let isPrevOp() =
        let isOp(nloc) =
            let t = Loc.current(nloc)
            match t with
            | {Text = txt; Kind = TokenKind.Operator} when
                txt.StartsWith("&") ||
                txt.StartsWith("|") -> true
            | _ -> false

        isNextToken(isOp, Loc.LEFT, findEOL(Loc.LEFT, loc))

    let tok = Loc.current(loc)
    if isEOL(Loc.LEFT, loc) &&
       isSpace(Loc.SELF, loc) &&
       not <| isComment(Loc.RIGHT, loc) &&
       not <| isPrevOp()
    then
        let dx = state.OldSize - tok.Text.Length
        let abs = System.Math.Abs(dx)
        if dx < 0 && abs > MaxShiftLength then
            true
        else
            let x = if state.RuleType = DoBlock then 4 else 2
            abs < MaxShiftLength - x
    else
        true

let isObjectExpressionBegin(loc) =
    let isWithTok(nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "with"; Kind = TokenKind.Keyword} -> true
        | _ -> false

    let isMatch(nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "{"; Kind = TokenKind.Delimiter} -> true
        | _ -> false

    let isBreak(i, nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "}"; Kind = TokenKind.Delimiter}
        | {Text = "match"; Kind = TokenKind.Keyword}
        | {Text = "try"; Kind = TokenKind.Keyword} -> true
        | _ -> false

    let t = Loc.current(loc)
    match t with
    | SpaceOrComment -> false
    | _ ->
        isNextToken(isWithTok, Loc.LEFT, loc) &&
        isToken(isMatch, isBreak, Loc.LEFT, loc)

let isObjectExpressionEnd(state)(loc) =
    let isEnd() =
        let t = Loc.current(loc)
        match t with
        | {Text = "}"; Kind = TokenKind.Delimiter} -> true
        | _ -> false

    state.RuleType = ObjExp &&
    isEnd()

let isSeqScopeBegin(loc) =
    let t = Loc.current(loc)
    match t with
    | SeqClose
    | SpaceOrComment -> false
    | _ -> isNextToken(toPred(|SeqOpen|_|), Loc.LEFT, loc)

let isSeqScopeEnd(state)(loc) =
    let self() =
        let t = Loc.current(loc)
        match t with
        | SeqClose -> not <| isNextToken(toPred(|SeqOpen|_|), Loc.LEFT, loc)
        | _ -> false

    let right() =
        isEOL(Loc.SELF, loc) &&
        isNextToken(toPred(|SeqClose|_|), Loc.RIGHT, loc) &&
        not <| isNextToken(toPred(|SeqOpen|_|), Loc.SELF, loc)

    self() || right()

let isTupleScopeBegin(loc) =
    let t = Loc.current(loc)
    match t with
    | TupleClose
    | SpaceOrComment -> false
    | _ -> isNextToken(toPred(|TupleOpen|_|), Loc.LEFT, loc)

let isTupleScopeEnd(state)(loc) =
    let self() =
        isNextToken(toPred(|TupleClose|_|), Loc.SELF, loc) &&
        not <| isNextToken(toPred(|TupleOpen|_|), Loc.LEFT, loc)

    let right() =
        isEOL(Loc.SELF, loc) &&
        isNextToken(toPred(|TupleClose|_|), Loc.RIGHT, loc) &&
        not <| isNextToken(toPred(|TupleOpen|_|), Loc.SELF, loc)

    self() || right()

let isMatchScopeBegin(loc) =
    let t = Loc.current(loc)
    match t with
    | {Text = "match"; Kind = TokenKind.Keyword} -> true
    | _ -> false

let isMatchScopeEnd(state)(loc) =
    let isPipe(nloc) =
        isNextToken(toPred(|PipeOperator|_|), Loc.RIGHT, nloc) ||
        isNextToken(toPred(|PipeOperator|_|), Loc.SELF, nloc)

    let isPipeExp(nloc) =
        let isMatchPipe(nloc) =
            let t = Loc.current(nloc)
            match t with
            | PipeOperator -> true
            | _ -> false

        let isMatchArrow(nloc) =
            let t = Loc.current(nloc)
            match t with
            | {Text = "->"; Kind = TokenKind.Keyword} -> true
            | _ -> false

        let isBreakPipe(i, nloc) =
            isMatchArrow(nloc)

        let isBreakArrow(i, nloc) =
            isMatchPipe(nloc)

        isTokenWith(isMatchPipe, isBreakPipe, Loc.LEFT, nloc, 1024) &&
        isTokenWith(isMatchArrow, isBreakArrow, Loc.RIGHT, nloc, 1024)

    let isEnd() =
        if not <| isEOL(Loc.LEFT, loc) then
            false
        else if isMatchScopeBegin(loc) then
            false
        else if isComment(Loc.RIGHT, loc) then
            false
        else if not <| isPipe(loc) then
            true
        else if not <| isWithinThreshold(state, loc) then
            true
        else
            false

    state.RuleType = Match &&
    (isEnd() ||
     isSeqScopeEnd(state)(loc) ||
     isTupleScopeEnd(state)(loc)) &&
    not <| isPipeExp(loc)

let isUnit(nloc) =
    let isOpen(nloc2) =
        let t = Loc.current(nloc2)
        match t with
        | {Text = "("; Kind = TokenKind.Delimiter} -> true
        | _ -> false
    let isClose(nloc2) =
        let t = Loc.current(nloc2)
        match t with
        | {Text = ")"; Kind = TokenKind.Delimiter} -> true
        | _ -> false

    isNextToken(isOpen, Loc.RIGHT, nloc) &&
    isNextToken(isClose, Loc.RIGHT, findNotEmpty(Loc.RIGHT, nloc))

let isBlockScopeBegin(loc) =
    let isLetModifier(text) =
        match text with
        | "public"
        | "private"
        | "internal"
        | "rec"
        | "inline" -> true
        | _ -> false

    let rec isLet(nloc) =
        let t, nloc2 = next(Loc.LEFT, nloc)
        match t with
        | {Text = "member"; Kind = TokenKind.Keyword}
        | {Text = "let!"; Kind = TokenKind.Keyword}
        | {Text = "let"; Kind = TokenKind.Keyword} -> true
        | SeqOpen -> false
        | {Text = text; Kind = TokenKind.Keyword} when not <|isLetModifier(text) -> false
        | _ when not <| isNext(Loc.LEFT, nloc2) -> false
        | _ -> isLet(nloc2)

    let isBlock(nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "function"; Kind = TokenKind.Keyword}
        | {Text = "yield!"; Kind = TokenKind.Keyword}
        | {Text = "yield"; Kind = TokenKind.Keyword}
        | {Text = "do!"; Kind = TokenKind.Keyword}
        | {Text = "then"; Kind = TokenKind.Keyword}
        | {Text = "else"; Kind = TokenKind.Keyword}
        | {Text = "->"; Kind = TokenKind.Keyword} when not <| isUnit(nloc) -> true
        | {Text = "="; Kind = TokenKind.Operator} when isLet(nloc) -> true
        | _ -> false

    let t = Loc.current(loc)
    match t with
    | SpaceOrComment -> false
    | _ -> isNextToken(isBlock, Loc.LEFT, loc)

let isBlockScopeEnd(state)(loc) =
    let isEnd() = not <| isWithinThreshold(state, loc)

    state.RuleType = Block &&
    (isEnd() ||
     isSeqScopeEnd(state)(loc) ||
     isTupleScopeEnd(state)(loc))

let isDoBlockScopeBegin(loc) =
    let isBlock(nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "do"; Kind = TokenKind.Keyword} when not <| isUnit(nloc) -> true
        | _ -> false

    let t = Loc.current(loc)
    match t with
    | SpaceOrComment -> false
    | _ -> isNextToken(isBlock, Loc.LEFT, loc)

let isDoBlockScopeEnd(state)(loc) =
    let isEnd() = not <| isWithinThreshold(state, loc)

    state.RuleType = DoBlock &&
    (isEnd() ||
     isSeqScopeEnd(state)(loc) ||
     isTupleScopeEnd(state)(loc))

let isIfScopeBegin(loc) =
    let isBlock(nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "if"; Kind = TokenKind.Keyword} -> true
        | _ -> false

    let t = Loc.current(loc)
    match t with
    | SpaceOrComment -> false
    | _ -> isNextToken(isBlock, Loc.LEFT, loc)

let isIfScopeEnd(state)(loc) =
    let isBlock(nloc) =
        let t = Loc.current(nloc)
        match t with
        | {Text = "then"; Kind = TokenKind.Keyword} -> true
        | _ -> false

    state.RuleType = IfThen &&
    isNextToken(isBlock, Loc.RIGHT, loc)

let isIndentException(loc, state, ts) =
    let rec isMatch(xs) =
        match xs with
        | (_, x) :: ts ->
            if x.RuleType = Match then
                true
            else if x.RuleType = Seq || x.RuleType = Tuple then
                isMatch(ts)
            else
                false
        | [] -> false

    (state.RuleType = Seq || state.RuleType = Tuple) &&
    isNextToken(toPred(|PipeOperator|_|), Loc.RIGHT, loc) &&
    isMatch(ts)

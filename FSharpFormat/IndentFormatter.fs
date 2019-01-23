module private FSharpFormat.IndentFormatter

let private formatIndent(loc, indents: list<(EndRule * IndentState)>) =
    let isIndent(loc) =
        let t, _ = next(Loc.RIGHT, loc)
        match t with
        | SpaceOrComment -> false
        | _ ->
            isEOL(Loc.LEFT, loc) &&
            isSpace(Loc.SELF, loc)

    match indents with
    | (isEnd, state) :: ts when isIndent(loc) ->
        let srcSize = getIndentSize(Loc.next(loc))
        let diff = System.Math.Abs(srcSize - state.NewSize)

        if diff = 0 ||
           not <| state.IsShifted ||
           isIndentException(loc, state, ts)
        then
            loc, (isEnd, {state with IsShifted = false}) :: ts
        else
            let newIndent = String.replicate state.NewSize " "
            let nloc =
                Loc.insertBefore(
                    {Text = newIndent; Kind = TokenKind.WhiteSpace; LeftColumn = 0},
                    Loc.removeBefore(loc))
                |> Loc.prev

            nloc, (isEnd, {state with IsShifted = true}) :: ts

    | _ -> loc, indents

let private findIndent(loc, indents: list<(EndRule * IndentState)>, rules: list<(BeginRule * EndRule * IndentRuleType)>) =
    let scopeBegin(xs): list<(EndRule * IndentState)> =
        let isScopeBegin(isBegin, _, _) = isNextToken(isBegin, Loc.SELF, loc)
        let getNextWord() =
            if isEOL(Loc.SELF, loc) then
                findNotEmpty(Loc.RIGHT, loc)
            else
                loc

        match rules |> List.tryFind(isScopeBegin) with
        | Some(_, isEnd, ruleType) ->
            let nw = getNextWord()
            let ct = Loc.current(nw)
            let cs = getIndentSize(nw)

            let state = {
                NewSize = cs
                OldSize = ct.LeftColumn
                IsShifted = cs <> ct.LeftColumn
                RuleType = ruleType
            }
            (isEnd, state) :: xs
        | None -> xs

    let rec scopeEnd(xs): list<(EndRule * IndentState)> =
        match xs with
        | [] -> xs
        | (isEnd, state) :: ts ->
            if isNextToken(isEnd(state), Loc.SELF, loc) then
                if state.RuleType = Seq || state.RuleType = Tuple then
                    ts
                else
                    scopeEnd(ts)
            else
                xs

    indents
    |> scopeBegin
    |> scopeEnd

let formatWith(rules)(loc, indents) =
    let indents = findIndent(loc, indents, rules)
    formatIndent(loc, indents)

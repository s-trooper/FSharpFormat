namespace FSharpFormat
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler.SourceCodeServices

type SourceTokenizer () =
    let lineSplitter = Regex("(?<line>[^\r\n]*)(?<break>[\r\n]*)", RegexOptions.Compiled)
    let tokenizer = FSharpSourceTokenizer([], None)
    let MAX_TOKENS = 2048

    let enableInactiveCode(line: string, nstate: FSharpTokenizerLexState) =
        let tl = line.Trim()
        match nstate with
        | 4294967299L
        | 4295000065L
        | 32769L when tl.StartsWith("#else") || tl.StartsWith("#endif")-> 4294967299L  // enable
        | 4294967299L
        | 4295000065L -> 32769L // disable
        | _ -> nstate

    let tokenizeLine (line: string, state: int64): FSharpTokenInfo list * int64 =
        let stateId = enableInactiveCode(line, state)

        let lineTokenizer = tokenizer.CreateLineTokenizer(line)
        let mutable nstate = (None, stateId)
        let tokens =
            [
            while (nstate <- lineTokenizer.ScanToken(snd nstate); (fst nstate).IsSome) do
                yield (fst nstate).Value
            ]
        tokens, snd(nstate)

    let fixMultiTokenLength(t, ts: list<FSharpTokenInfo>, i) =
        let thisLen = t.RightColumn - t.LeftColumn
        if t.ColorClass = FSharpTokenColorKind.Punctuation && thisLen <> t.FullMatchedLength - 1 then
            let mutable opLen = 0
            let mutable skip = 0

            for t in ts.[i .. i + t.FullMatchedLength - 2] do
                opLen <- opLen + t.RightColumn - t.LeftColumn + 1
                if opLen < t.FullMatchedLength then
                    skip <- skip + 1
            skip
        else
            0

    let fixBananaClipLength(that, next) =
        if that.TokenName = "LPAREN" && next.TokenName = "BAR" ||
           that.TokenName = "BAR" && next.TokenName = "RPAREN" then
            1
        else
            0
    let fixHashToken(that, line: string) =
        if that.TokenName = "HASH" && that.LeftColumn = 0 then
            if line.Length > 1 && line.Substring(1, 1) = " " then
                1
            else
                0
        else
            0
    let fixArrayToken(str, kind) =
        if str = ">|]" && kind = Delimiter then
            true
        else
            false

    let createTok(tokens: list<_>, line: string) =
        let result = ResizeArray()
        let count = Seq.length(tokens)
        let mutable i = 0

        // TODO: perf optimation
        if count > MAX_TOKENS then
            result.Add({Text = line; Kind = TokenKind.Text; LeftColumn = 0})
            i <- count

        while i < count do
            try
                let t = tokens.[i]
                let nexti = if i + 1 >= count then count - 1 else i + 1
                let blen = fixBananaClipLength(t, tokens.[nexti])
                let mlen = fixMultiTokenLength(t, tokens, i)
                let hlen = fixHashToken(t, line)

                let len = if mlen = 0 then t.RightColumn - t.LeftColumn + 1 else t.FullMatchedLength
                let str = line.Substring(t.LeftColumn, len + blen + hlen)

                let kind =
                    match t.ColorClass, t.CharClass with
                    | FSharpTokenColorKind.PreprocessorKeyword, _ -> TokenKind.Keyword
                    | FSharpTokenColorKind.InactiveCode, _ -> TokenKind.Default
                    | _, _ when mlen > 0 -> TokenKind.Operator
                    | _, FSharpTokenCharKind.Keyword -> TokenKind.Keyword
                    | _, FSharpTokenCharKind.Identifier -> TokenKind.Identifier
                    | _, FSharpTokenCharKind.Literal -> TokenKind.Literal
                    | _, FSharpTokenCharKind.Operator -> TokenKind.Operator
                    | _, FSharpTokenCharKind.Delimiter -> TokenKind.Delimiter
                    | _, FSharpTokenCharKind.WhiteSpace -> TokenKind.WhiteSpace
                    | _, FSharpTokenCharKind.String
                    | _, FSharpTokenCharKind.Text -> TokenKind.Text
                    | _, FSharpTokenCharKind.LineComment
                    | _, FSharpTokenCharKind.Comment -> TokenKind.Comment
                    | _, _-> TokenKind.Default

                if fixArrayToken(str, kind) then
                    let tok = {Text = ">"; Kind = TokenKind.Operator; LeftColumn = t.LeftColumn}
                    result.Add(tok)
                    let tok = {Text = "|]"; Kind = kind; LeftColumn = t.LeftColumn + 1}
                    result.Add(tok)
                else
                    let tok = {Text = str; Kind = kind; LeftColumn = t.LeftColumn}
                    result.Add(tok)

                i <- i + 1 + mlen + blen

            with ex ->
                failwithf "Line: %s\n%A" line ex

        result

    member __.Tokenize (source: string): Token list =
        let matches = lineSplitter.Matches(source)
        let mutable state = 0L
        [
        for lineMatch in matches do
            let line = lineMatch.Groups.["line"].Value
            let tokens, nstate = tokenizeLine(line, state)
            state <- nstate

            for tok in createTok(tokens, line) do
                yield tok

            let breakText = lineMatch.Groups.["break"].Value
            if breakText.Length <> 0 then
                yield {
                    Text = breakText
                    Kind = TokenKind.EOL
                    LeftColumn = line.Length - breakText.Length
                }
        ]

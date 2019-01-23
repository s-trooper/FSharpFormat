namespace FSharpFormat
open System.IO

type Formatter = TokenChain -> TokenChain

type SourceFormatter (formatters: Formatter list) =
    let tokenizer = SourceTokenizer()

    let formatToken tokens formatter =
        formatter(tokens)
        |> Loc.moveToBegin

    let formatWith (formatters: Formatter list) (tokens: Token list): Token list =
        let tokenChain = Loc.create(tokens)
        formatters
        |> Seq.fold formatToken tokenChain
        |> Loc.toList

    let writeToken (writer: TextWriter) token =
        match token with
        | {Text = txt} -> writer.Write(txt)

    let toString (tokens) =
        use writer = new StringWriter()
        tokens
        |> Seq.iter (writer |> writeToken)

        writer.ToString()

    member __.Format(source: string): string =
        tokenizer.Tokenize(source)
        |> formatWith formatters
        |> toString

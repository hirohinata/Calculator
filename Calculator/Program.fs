open System
open Antlr4.Runtime
open Calculator

[<EntryPoint>]
let main argv =
    let expression = argv.[0]
    let stream = new AntlrInputStream(expression)
    let lexer = new ExpressionLexer(stream)
    let parser = new ExpressionParser(new CommonTokenStream(lexer))
    let visitor = new Visitor()
    match visitor.Visit(parser.input()) with
    | Result.Integer value -> Console.WriteLine(value)
    | Result.Real value -> Console.WriteLine(value)
    | Result.String text -> Console.WriteLine(text)
    | Result.Error -> Console.WriteLine("ERROR")
    0

open Antlr4.Runtime
open Calculator

[<EntryPoint>]
let main argv =
    let expression = argv.[0]
    let stream = new AntlrInputStream(expression)
    let lexer = new ExpressionLexer(stream)
    let parser = new ExpressionParser(new CommonTokenStream(lexer))
    let visitor = new Visitor()
    let result = visitor.Visit(parser.input())
    0

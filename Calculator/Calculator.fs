namespace Calculator

open Antlr4.Runtime

module public Calculator =
    [<CompiledName "Execute">]
    let execute (expression:string) =
        let stream = new AntlrInputStream(expression)
        let lexer = new ExpressionLexer(stream)
        let parser = new ExpressionParser(new CommonTokenStream(lexer))
        let visitor = new Visitor()
        try
            match visitor.Visit(parser.input()) with
            | Result.Integer value -> "CalcInt:" + value.ToString()
            | Result.Real value -> "CalcReal:" + value.ToString()
            | Result.String text -> "CalcString:" + text
            | Result.Error err ->
                match err with
                | UnSupportCalcRule -> "Err.UnSupportCalcRule"
                | ZeroDiv -> "Err.ZeroDiv"
                | OverFlow -> "Err.OverFlow"
                | CantMinusString -> "Err.CantMinusString"
        with
        | e -> "Err.Exception ( " + e.Message + " )"
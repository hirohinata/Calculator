namespace Calculator

type Visitor() =
    inherit ExpressionBaseVisitor<Result>()
        override this.DefaultResult = Result.Error
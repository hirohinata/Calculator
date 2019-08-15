namespace Calculator

type Visitor() =
    inherit ExpressionBaseVisitor<unit>()
    
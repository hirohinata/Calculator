namespace Calculator

open Antlr4.Runtime.Misc

type Visitor() =
    inherit ExpressionBaseVisitor<Result>()
        override this.DefaultResult = Result.Error

        override this.VisitInput([<NotNull>]context: ExpressionParser.InputContext) =
            context.expr() |> this.Visit

        override this.VisitAddExpr([<NotNull>]context: ExpressionParser.AddExprContext) =
            let realOp lhs rhs = Checked.(+) lhs rhs |> Result.Real

            match context.lhs |> this.Visit, context.rhs |> this.Visit with
            | Integer lhs, Integer rhs -> Checked.(+) lhs rhs |> Result.Integer
            | Integer lhs, Real rhs -> realOp (double lhs) rhs
            | Integer _, (String _ | Error) -> Error
            | Real lhs, Integer rhs -> realOp lhs (double rhs)
            | Real lhs, Real rhs -> realOp lhs rhs
            | Real _, (String _ | Error) -> Error
            | String lhs, String rhs -> (+) lhs rhs |> String
            | String _, (Integer _ | Real _ | Error) -> Error
            | Error, (Integer _ | Real _ | String _ | Error) -> Error

        override this.VisitSubExpr([<NotNull>]context: ExpressionParser.SubExprContext) =
            let realOp lhs rhs = Checked.(-) lhs rhs |> Result.Real

            match context.lhs |> this.Visit, context.rhs |> this.Visit with
            | Integer lhs, Integer rhs -> Checked.(-) lhs rhs |> Result.Integer
            | Integer lhs, Real rhs -> realOp (double lhs) rhs
            | Integer _, (String _ | Error) -> Error
            | Real lhs, Integer rhs -> realOp lhs (double rhs)
            | Real lhs, Real rhs -> realOp lhs rhs
            | Real _, (String _ | Error) -> Error
            | String lhs, String rhs -> invalidOp "CantMinusString"
            | String _, (Integer _ | Real _ | Error) -> Error
            | Error, (Integer _ | Real _ | String _ | Error) -> Error

        override this.VisitMultiExpr([<NotNull>]context: ExpressionParser.MultiExprContext) =
            let realOp lhs rhs = Checked.( * ) lhs rhs |> Result.Real

            match context.lhs |> this.Visit, context.rhs |> this.Visit with
            | Integer lhs, Integer rhs
                -> Checked.( * ) lhs rhs |> Result.Integer
            | Integer lhs, Real rhs
                -> realOp (double lhs) rhs
            | Real lhs, Integer rhs
                -> realOp lhs (double rhs)
            | Real lhs, Real rhs
                -> realOp lhs rhs
            | Integer times, String text
            | String text, Integer times
                -> List.fold (fun acc _ -> acc + text) "" [1 .. times] |> String
            | Integer _, Error
            | Real _, (String _ | Error)
            | String _, (Real _ | String _ | Error)
            | Error, (Integer _ | Real _ | String _ | Error)
                -> Error

        override this.VisitPlusExpr([<NotNull>]context: ExpressionParser.PlusExprContext) =
            match context.rhs |> this.Visit with
            | Integer value -> value |> Integer
            | Real value -> value |> Real
            | String _ -> invalidOp "CantUnaryPlusString"
            | Error -> Error

        override this.VisitMinusExpr([<NotNull>]context: ExpressionParser.MinusExprContext) =
            match context.rhs |> this.Visit with
            | Integer value -> -value |> Integer
            | Real value -> -value |> Real
            | String _ -> invalidOp "CantUnaryMinusString"
            | Error -> Error

        override this.VisitUintLiteral([<NotNull>]context: ExpressionParser.UintLiteralContext) =
            context.UINT().Symbol.Text |> System.Int32.Parse |> Integer

        override this.VisitRealLiteral([<NotNull>]context: ExpressionParser.RealLiteralContext) =
            context.REAL().Symbol.Text |> System.Double.Parse |> Real

        override this.VisitStringLiteral([<NotNull>]context: ExpressionParser.StringLiteralContext) =
            let text = context.STRING().Symbol.Text
            text.Substring(1, text.Length - 2) |> String

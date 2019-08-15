namespace Calculator

open Antlr4.Runtime.Misc

type Visitor() =
    inherit ExpressionBaseVisitor<Result>()
        override this.DefaultResult = Result.Error

        override this.VisitInput([<NotNull>]context: ExpressionParser.InputContext) =
            context.expr() |> this.Visit

        override this.VisitAddExpr([<NotNull>]context: ExpressionParser.AddExprContext) =
            let lhs = context.lhs |> this.Visit
            let rhs = context.rhs |> this.Visit
            let realOp lhs rhs = Checked.(+) lhs rhs |> Result.Real

            match lhs, rhs with
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
            let lhs = context.lhs |> this.Visit
            let rhs = context.rhs |> this.Visit
            let realOp lhs rhs = Checked.(-) lhs rhs |> Result.Real

            match lhs, rhs with
            | Integer lhs, Integer rhs -> Checked.(-) lhs rhs |> Result.Integer
            | Integer lhs, Real rhs -> realOp (double lhs) rhs
            | Integer _, (String _ | Error) -> Error
            | Real lhs, Integer rhs -> realOp lhs (double rhs)
            | Real lhs, Real rhs -> realOp lhs rhs
            | Real _, (String _ | Error) -> Error
            | String lhs, String rhs -> invalidOp "CantMinusString"
            | String _, (Integer _ | Real _ | Error) -> Error
            | Error, (Integer _ | Real _ | String _ | Error) -> Error

        override this.VisitUintLiteral([<NotNull>]context: ExpressionParser.UintLiteralContext) =
            context.UINT().Symbol.Text |> System.Int32.Parse |> Integer

        override this.VisitRealLiteral([<NotNull>]context: ExpressionParser.RealLiteralContext) =
            context.REAL().Symbol.Text |> System.Double.Parse |> Real

        override this.VisitStringLiteral([<NotNull>]context: ExpressionParser.StringLiteralContext) =
            let text = context.STRING().Symbol.Text
            text.Substring(1, text.Length - 2) |> String

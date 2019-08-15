namespace Calculator

open Antlr4.Runtime.Misc

type Visitor() =
    inherit ExpressionBaseVisitor<Result>()
        override this.DefaultResult = Result.Error

        override this.VisitInput([<NotNull>]context: ExpressionParser.InputContext) =
            context.expr() |> this.Visit

        override this.VisitExpr_additive([<NotNull>]context: ExpressionParser.Expr_additiveContext) =
            let lhs = context.lhs |> this.Visit
            let rhs = context.rhs |> this.Visit

            let intOp lhs rhs =
                match context.op.Type with
                | ExpressionParser.PLUS -> Checked.(+) lhs rhs |> Result.Integer
                | ExpressionParser.MINUS -> Checked.(-) lhs rhs |> Result.Integer
                | _ -> invalidOp ""
            let realOp lhs rhs =
                match context.op.Type with
                    | ExpressionParser.PLUS -> Checked.(+) lhs rhs
                    | ExpressionParser.MINUS -> Checked.(-) lhs rhs
                    | _ -> invalidOp ""
                |> Result.Real
            let strOp lhs rhs =
                match context.op.Type with
                | ExpressionParser.PLUS -> (+) lhs rhs |> String
                | ExpressionParser.MINUS -> Error
                | _ -> invalidOp ""

            match lhs, rhs with
            | Integer lhs, Integer rhs -> intOp lhs rhs
            | Integer lhs, Real rhs -> realOp (double lhs) rhs
            | Integer _, (String _ | Error) -> Error
            | Real lhs, Integer rhs -> realOp lhs (double rhs)
            | Real lhs, Real rhs -> realOp lhs rhs
            | Real _, (String _ | Error) -> Error
            | String lhs, String rhs -> strOp lhs rhs
            | String _, (Integer _ | Real _ | Error) -> Error
            | Error, (Integer _ | Real _ | String _ | Error) -> Error

        override this.VisitNum_uint([<NotNull>]context: ExpressionParser.Num_uintContext) =
            context.UINT().Symbol.Text |> System.Int32.Parse |> Integer

        override this.VisitNum_real([<NotNull>]context: ExpressionParser.Num_realContext) =
            context.REAL().Symbol.Text |> System.Double.Parse |> Real

        override this.VisitNum_string([<NotNull>]context: ExpressionParser.Num_stringContext) =
            let text = context.STRING().Symbol.Text
            text.Substring(1, text.Length - 2) |> String

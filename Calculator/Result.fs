namespace Calculator

type Result =
    | Integer of Value : int32
    | Real of Value : double
    | String of Text : string
    | Error of Error
and Error =
    | UnSupportCalcRule
    | ZeroDiv
    | OverFlow
    | CantMinusString
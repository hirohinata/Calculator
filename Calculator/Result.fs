namespace Calculator

type Result =
    | Integer of Value : int64
    | Real of Value : double
    | String of Text : string
    | Error
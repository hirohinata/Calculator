open System

[<EntryPoint>]
let main argv =
    Console.WriteLine(Calculator.Calculator.execute argv.[0])
    0

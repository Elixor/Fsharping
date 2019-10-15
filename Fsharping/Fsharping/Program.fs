// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    GameOfLife.entryPoint argv |> ignore

    0 // return an integer exit code

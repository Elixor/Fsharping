module GameOfLife

open System

let size = 10

let add x y = x + y   

let getReal x = 
    if x < 0 then x + size
    elif x > (size - 1) then x - size
    else x

let colorValue valueIndex value vectorIndex (matrixAfter : int list list) =
    if value = 1 && matrixAfter.[vectorIndex].[valueIndex] = 1 then "o>o"
    elif value = 1 && matrixAfter.[vectorIndex].[valueIndex] = 0 then "o>x"
    elif value = 0 && matrixAfter.[vectorIndex].[valueIndex] = 1 then "x>o"
    else "x>x"

let colorVector vectorIndex vector matrixAfter =
    vector
    |> List.mapi (fun valueIndex value -> colorValue valueIndex value vectorIndex matrixAfter )

let nextValue valueIndex value vectorIndex (matrix : int list list) =
    let sousedi = [
        yield matrix.[getReal (vectorIndex - 1)].[getReal (valueIndex - 1)];
        yield matrix.[getReal (vectorIndex - 1)].[getReal valueIndex];
        yield matrix.[getReal (vectorIndex - 1)].[getReal (valueIndex + 1)];
        yield matrix.[getReal vectorIndex].[getReal (valueIndex - 1)];
        yield matrix.[getReal vectorIndex].[getReal (valueIndex + 1)];
        yield matrix.[getReal (vectorIndex + 1)].[getReal (valueIndex - 1)];
        yield matrix.[getReal (vectorIndex + 1)].[getReal valueIndex]
        yield matrix.[getReal (vectorIndex + 1)].[getReal (valueIndex + 1)]]

    let sousediSum = List.sum sousedi
    if value = 1 && sousediSum > 3 then 0
    elif value = 1 && (sousediSum = 2 || sousediSum = 3) then 1
    elif sousediSum < 2 then 0
    else 1

let nextVector vectorIndex vector matrix =
    vector
    |> List.mapi (fun valueIndex value -> nextValue valueIndex value vectorIndex matrix )

let printOutput (matrix : string list list) =
    let twoDimensionalArray = Array2D.init size size (fun i j -> matrix.[i].[j])
    printfn "Oldova hra (o) život!"
    twoDimensionalArray |> printfn "%A"
    printfn "Další generace?"
    Console.ReadKey() |> ignore
    System.Console.Clear()
    

let nextMatrix matrix =
    let newMatrix = matrix |> List.mapi (fun vectorIndex vector -> nextVector vectorIndex vector matrix)
    let matrixAfter = newMatrix |> List.mapi (fun vectorIndex vector -> nextVector vectorIndex vector newMatrix)
    newMatrix 
    |> List.mapi (fun vectorIndex vector -> colorVector vectorIndex vector matrixAfter)
    |> printOutput 
    |> ignore
    newMatrix

let rec infiniteLoop (matrix : int list list) =
    matrix
    |> nextMatrix 
    |> infiniteLoop


let entryPoint args =
    let random x =
        let random = new System.Random()
        (random.Next(1, (size * 1000)))%2
    
    [ for i in 1 .. size -> [1..size] |> List.map random]
    |> infiniteLoop
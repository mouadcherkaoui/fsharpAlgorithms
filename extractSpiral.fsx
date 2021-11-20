open System
open System.Collections


let source = 
    array2D [|
        [| 1; 2; 3; 4; 5 |];
        [| 16; 17; 18; 19; 6 |];
        [| 15; 24; 25; 20; 7 |];
        [| 14; 23; 22; 21; 8 |];
        [| 13; 12; 11; 10; 9 |];                                
    |]

let moveX start _end y (arr: int[,]) =
    if start > _end then
        [|for i = start downto _end do
            arr.[i, y]|]
    else
        [|for i = start to _end do
            arr.[i, y]|]


let moveY start _end x (arr: int[,]) =
    if start > _end then
        [|for i = start downto _end do
            arr.[x, i]|]
    else
        [|for i = start to _end do
            arr.[x, i]|]

let mutable counter = 0
                        
let printResult (arr: int array) = 
    Array.ForEach(arr, (fun v ->  printfn "%d" v))

let incrementCounter amount = 
    counter <- counter + amount

let extract (arr: int[,]) = 
    
    let mutable result: int[] = [||] // Array.CreateInstance(typeof<int>, int64(arr.Length) ) 
    let mutable length = (arr.GetUpperBound 1) 
    let mutable lvl = 0
    for i = 0 to (length - 1) do 
        if counter < arr.Length then
            lvl <- length - i
            moveY i (lvl - 1) i arr
                |> fun res -> 
                    printResult res
                    incrementCounter res.Length   
                    result <- Array.append result res                    
            moveX i (lvl - 1) lvl arr
                |> fun res -> 
                    printResult res
                    incrementCounter res.Length
                    result <- Array.append result res
            moveY (lvl) (i + 1) lvl arr
                |> fun res -> 
                    printResult res
                    incrementCounter res.Length                                
                    result <- Array.append result res
            moveX (lvl) (i + 1) i arr
                |> fun res -> 
                    printResult res
                    incrementCounter res.Length
                    result <- Array.append result res
    result

extract source
// |> printfn "%A"


// For more information see https://aka.ms/fsharp-console-apps
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

let source2 =
    array2D [|
        [| 1; 2; 3; 4; 5; 6; 7 |];
        [| 24; 25; 26; 27; 28; 29; 8 |];
        [| 23; 40; 41; 42; 43; 30; 9 |];
        [| 22; 39; 48; 49; 44; 31; 10 |];
        [| 21; 38; 47; 46; 45; 32; 11 |];
        [| 20; 37; 36; 35; 34; 33; 12 |];
        [| 19; 18; 17; 16; 15; 14; 13 |];

    |]

(*****************************************************************************

*****************************************************************************)

let printResult (arr: int array) =
    Array.ForEach(arr, (fun v ->  printfn "%d" v))

let reverse arr =
    Array.rev arr

(*****************************************************************************
******************************************************************************)

let moveX start _end y (arr: int[,]) =
    if start > _end then
        [|for i = start downto _end do
            arr.[i, y]|]
    else
        [|for i = start to _end do
            arr.[i, y]|]

let mvX start _end y (arr: int[,]) =
    if start > _end then
        reverse arr.[_end .. start, y]
    elif start < _end then
        arr.[start .. _end, y]
    else
        [|arr.[start, y]|]

let moveY start _end x (arr: int[,]) =
    if start > _end then
        [|for i = start downto _end do
            arr.[x, i]|]
    else
        [|for i = start to _end do
            arr.[x, i]|]

let mvY start _end x (arr: int[,]) =
    if start > _end then
        reverse arr.[x, _end .. start]
    elif start < _end then
        arr.[x, start .. _end]
    else
        [|arr.[x, start]|]

let extract (arr: int[,]) = 
    
    let mutable result: int[] = [||]
    let mutable length = (arr.GetUpperBound 1) 
    let mutable level = 0

    let appendToResult (range: int[]) =
        result <- Array.append result range                


    for i = 0 to (length - 1) do 
            level <- length - i
            mvY i (level - 1) i arr // 1
                |> appendToResult                    
            mvX i (level - 1) level arr // 2
                |> appendToResult 
            mvY (level) (i + 1) level arr // 3
                |> appendToResult
            mvX (level) (i + 1) i arr // 4
                |> appendToResult
    done
    result.[0..arr.Length - 1]

extract source2
|> fun result ->
    printfn "%A" result
    result

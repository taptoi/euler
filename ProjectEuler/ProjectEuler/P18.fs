﻿module P18
open System
let input = 
   "75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

let lines (inp:string) = inp.Split('\n') |> Array.toList
let rows (ls : string list) = 
            ls  |> List.map (fun s -> s.Split(' '))
                |> List.map (fun a -> Array.toList(a) |> List.map (Int32.Parse))

let pairs s = s |> List.pairwise 

let reducePairs ps = ps |> List.map (fun (a,b) -> max a b )

let solve' ls =
    let rec reduce (rows : int list list) =
        match rows with
        | [] -> 0
        | x :: xs ->
            match xs with 
            | [] -> List.exactlyOne x
            | _ ->    
                    let children = x
                    let parents = List.head xs
                    let children' = children |> pairs |> reducePairs
                    let parents' = (List.zip parents children')
                                   |> List.map (fun (parent, child) -> parent + child)
                    reduce (parents' :: (List.tail xs))
            
    ls 
        |> rows 
        |> List.rev
        |> reduce

let solve = solve' (input |> lines)
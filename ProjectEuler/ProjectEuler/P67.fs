module P67
open P18
open System.IO

let solve =
        (File.ReadLines "triangle.txt")
        |> Seq.toList
        |> solve'

module P16
open System

let rec nthPowOfTwo n (acc : double) =
    if n = 0 then acc
    else nthPowOfTwo (n-1) (acc * 2.0)


let round (x:double) = 
    x 
    |> System.Math.Round
    |> int

let allDigitsOf (v : double) =
    let rec nextDigit (d : double) =
       let curr = d.ToString().ToCharArray().[0]
       seq{
           yield curr
           let next = d / 10.
           if next > 0. then
                yield! nextDigit next
            }
    nextDigit v


let print (cs: char seq) =
    cs |> Seq.iter (fun c -> printf "%c" c)

let result() =
    nthPowOfTwo 10 1.0
    |> allDigitsOf
    |> print
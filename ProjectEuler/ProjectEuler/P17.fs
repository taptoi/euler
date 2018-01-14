module P17

open System

let getDigitAt (d : int) (i : int) =
    d.ToString().ToCharArray()
    |> Array.rev
    |> fun a -> sprintf "%c" a.[i]
    |> Int32.Parse

let toStringUnderTwenty i =
    match i with
    | 0 -> ""
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | _ -> failwith "Invalid input."

// Get the string indicating tens by i, so if i = 5, then return "fifty"
let tenS i =
    match i with
    | 1 -> "ten"
    | 2 -> "twenty"
    | 3 -> "thirty"
    | 4 -> "forty"
    | 5 -> "fifty"
    | 6 -> "sixty"
    | 7 -> "seventy"
    | 8 -> "eighty"
    | 9 -> "ninety"
    | _ -> failwith "Invalid input"

let hundredS = "hundred"
let thousandS = "one thousand"
let andS = "and"
let addWithSpace a = 
    let res b = a + " " + b 
    res

let sAdd s1 s2 = sprintf "%s %s" s1 s2

let toString i =
    let underTwenty a = toStringUnderTwenty a
    let underHundred a = 
        if a < 20 
        then 
            underTwenty a 
        else
            let tens = getDigitAt a 1 |> tenS
            let singles = getDigitAt a 0 |> toStringUnderTwenty
            sAdd tens singles
    if i < 20 then
        underTwenty i
    elif i < 100 then
        underHundred i
    elif i < 1000 then
        let hundreds = getDigitAt i 2 
        let modH = i % 100
        let nHundred = sAdd (hundreds |> toStringUnderTwenty) hundredS
        let nHundredAnd = sAdd nHundred andS
        if modH <> 0 then
            sAdd nHundredAnd (underHundred modH)
        else
            nHundred
    else 
        thousandS
    

let countLetters (s:string) =
    s.Replace (" ", "")
    |> fun s' -> s'.ToCharArray().Length

let solve =
    seq{ 1 .. 1000}
    |> Seq.map (fun i -> toString i)
    |> Seq.fold (fun acc elem -> addWithSpace acc elem) ""
    |> countLetters

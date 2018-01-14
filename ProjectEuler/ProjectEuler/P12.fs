module P12

let sq (n : int64) = int64 (sqrt (float n))
let getDivisors n =
    let sqrtn = sq n
    let simpleSeq = seq{ for i in 1L .. sqrtn do
                         if n % i = 0L then yield i }
    let pairedSeq = simpleSeq |> Seq.map (fun a -> n / a)
    Seq.append simpleSeq pairedSeq
    

let getTriangleNumber n =
    let rec getNextRec v acc =
        if v = n then acc + v
        else getNextRec ( v + 1L ) ( acc + v )
    getNextRec 1L 0L

let getTriangleNumberK n knownN knownNVal =
    let rec getNextRec v acc =
        if v = n then acc + v
        else getNextRec ( v + 1L ) ( acc + v )
    getNextRec (knownN + 1L) knownNVal

let result lim =
    let rec resultUntilLimit n limit knownN knownNVal =
        let num = getTriangleNumberK n knownN knownNVal
        let len = num |> getDivisors |> Seq.length
        if len < limit then resultUntilLimit ( n + 1L ) limit n num
        else num
    resultUntilLimit 2L lim 1L 1L

let solve = result 500
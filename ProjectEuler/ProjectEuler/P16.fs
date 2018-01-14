module P16
open System


let nthPowerOf2 n =
    let rec getNext (v : bigint) count =
        if count = n then 
            v * 2I
        else
            getNext (v * 2I) (count + 1)
    getNext 1I 1


let getMaxPowerOfTen (d : bigint) =
    let rec getNext curr n =
        let pow = curr * 10I
        if d / pow < 1I then
            n
        else getNext pow (n + 1I)
    getNext 1I 0I

let pow (x : bigint) (y : bigint) =
    if y = 0I then 1I else
        let rec getNext curr n =
            if (n = y) then curr
            else getNext (curr * x) (n + 1I)
        getNext x 1I



let getDigitsOf (v : bigint) =
    let powOfTen = getMaxPowerOfTen v
    let rec yieldNext a b c  = // 1024 0 3
        seq {
            let a' = a - b * (pow 10I (c + 1I)) // 1024
            let b' = a' / (pow 10I c) // 1
            let c' = c - 1I
            yield Some b'
            if c > 0I then
                yield! yieldNext a' b' c'
            else yield None
         }
    yieldNext v 0I powOfTen


let solve =
    nthPowerOf2 1000
    |> getDigitsOf
    |> Seq.choose id
    |> Seq.reduce (+)

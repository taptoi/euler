module P23

let isProperDivisor divident divisor =
    if divident > divisor && divident % divisor = 0  then true else false    

let properDivisors i = 
    seq { 1 .. i-1 }
    |> Seq.map( fun v -> if isProperDivisor i v then Some v else None )
    |> Seq.choose id

let sumProperDivisors i =
    properDivisors i 
    |> Seq.reduce (+)

// i It is abundant if the sum of its proper divisor is greater than i
let isAbundant i =
    if sumProperDivisors i > i then true else false

// Get sequence of all abundant numbers under limit s
let getAllAbundantNumbersUnder limit =
    seq{ 
        for i in 12 .. limit do
            if isAbundant i then yield i else ignore()
     }


// for i in s find the sum of all pairs i,u where i,u are in s
// return the distinct values of these
//let findSumsOfAllPairsInSeq s =
//    seq{
//        for i in s do
//            for j in s do
//                yield i + j
//    }
//    |> Seq.distinct

//1 2 3 4

//a0+a0
//a0+a1
//a0+a2
//a0+a3

//a1+a1
//a1+a2
//a1+a3

//a2+a2
//a2+a3

//a3+a3

let getAllSumsOfPairsInSeq (arr : int array) =
    seq{
        for i in 0 .. arr.Length - 1 do
            for j in i .. arr.Length - 1 do
                yield arr.[i] + arr.[j]
        }

// get all values in 1..limit 
let getAllSumsOfAbundantNumbersUnder limit =
    getAllAbundantNumbersUnder limit
    |> Seq.toArray
    |> getAllSumsOfPairsInSeq

let getAllIntegersThatAreNotSumsOfAbundantNumbersUnder limit =
    let allSums = getAllSumsOfAbundantNumbersUnder(limit) |> Seq.toArray
    let sumTable = Array.create<bool> (limit * limit) false
    for sum in allSums do sumTable.[sum] <- true
    seq{
        for i in 1 .. limit do
            if not sumTable.[i]
                then yield Some i else yield None
    }
    |> Seq.choose id

//let limit = 28123

let res(limit) =
    getAllIntegersThatAreNotSumsOfAbundantNumbersUnder limit
    |> Seq.reduce (+)
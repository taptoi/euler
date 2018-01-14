module P21

//Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
//If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

//For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

//Evaluate the sum of all the amicable numbers under 10000.

let isProperDivisor divident divisor =
    if divident > divisor && divident % divisor = 0  then true else false    

let properDivisors i = 
    seq { 1 .. i-1 }
    |> Seq.map( fun v -> if isProperDivisor i v then Some v else None )
    |> Seq.choose id

let sumProperDivisors i =
    properDivisors i 
    |> Seq.reduce (+)

let isAmicable i =
    if i <= 200 then false else
        let candidate = sumProperDivisors i
        if candidate = 1 then false else
            let sumProperDivisorsCandidate = sumProperDivisors candidate
            if candidate <> i &&
               sumProperDivisorsCandidate = i
            then true else false

let allAmicableUnder i =
    seq { 200.. i-1 }
    |> Seq.map (fun v -> if isAmicable v then Some v else None)
    |> Seq.choose id

let sumAllAmicableUnder i =
    allAmicableUnder i
    |> Seq.reduce (+)

let solve = sumAllAmicableUnder 10000
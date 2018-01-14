namespace ProjectEuler

    open System
    module P3 =
        let testNum = 600851475143L
        let testNum2 = 13195L

        let sqrt (N: int64) = N |> float |> Math.Sqrt |> int64
        let isEven (N: int64) = Math.DivRem(N,2L) |> snd = 0L
        let div (a: int64) (b: int64) = Math.DivRem(a,b) |> fst
        let modulo (a: int64) (b: int64) = Math.DivRem(a,b) |> snd
        let isEvenlyDivisible (a:int64) (b:int64) = (modulo a b) = 0L

        // brute force primality check:
        let isprime (N: int64) =
            printfn "isPrime: %d" N
            let rec check i =
                i > (div N 2L) || ((modulo N i) <> 0L && check (i + 1L))
            check 2L

        let findHighestPrimeFactor (N: int64) =
            let rec findPrimeFactorLoop y =
                match isEven y with   // check evens
                | true -> findPrimeFactorLoop (y-1L)  // skip evens altogether for optimization else would use: if y = 2L then y else findPrimeFactorLoop (y-1L) 
                | false -> match isEvenlyDivisible N y with // test
                            | true -> if isprime(y) then y else findPrimeFactorLoop (y-2L) // test primality. continue if not prime.
                            | false -> findPrimeFactorLoop (y-2L)
            findPrimeFactorLoop (sqrt N)

        let solve = findHighestPrimeFactor 600851475143L





                
                
     
                
                

namespace ProjectEuler

module P7 =
    open System
    open System.Collections
    open P3

    // brute force primality check:
    let isprime (N: int64) =
        let rec check i =
            i > (div N 2L) || ((modulo N i) <> 0L && check (i + 1L))
        check 2L

    let loop =
        let mutable found = false
        let mutable count = 1L
        let mutable x = 3L
        while count < 10002L do
            if isprime x then count <- count + 1L; printfn "Found %dth prime: %d" count x
            x <- x + 1L

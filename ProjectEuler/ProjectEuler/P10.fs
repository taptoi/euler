namespace ProjectEuler
module P10 =

    open System
    open P3

    // Sieve of Eratosthenes
    let primes limit = 
        //let table = BitArray(limit + 1, true)
        let table = seq { for i in 0 .. limit do yield false } |> Seq.toArray // Initialize a truth table with false (not marked)
        let markTable p = 
            if (int64(p) * int64(p) > int64(Int32.MaxValue)) then do () else // All until Int32 Max has already been marked
            { p*p .. p .. (limit) } |> Seq.iter( fun x -> table.[x] <- true ) // mark all multiples of p greater than p
        let findFirstIndexNotMarkedGreaterThan p = 
            let numbers = seq{ (p + 1) .. limit }
            match numbers |> Seq.tryFindIndex(fun i -> table.[i] = false) with
            | Some index -> numbers |> Seq.nth(index) |> Some
            | None -> None
        let rec loop p =
            markTable p // mark all multiples of 
            match findFirstIndexNotMarkedGreaterThan p with
            | Some x -> loop x // find first number greater than p that is not marked (this is the next prime) and reiterate
            | None -> seq {for i in 2 .. limit do if table.[i] = false then yield int64(i) } |> Seq.toArray // return all primes (unmarked) in the end as an array of Int64's. Number 1 is not a prime.
        loop 2

    let result = (primes 2000000 |> Array.reduce (fun acc elem -> acc + elem))

namespace ProjectEuler

module P6 =
    open System
    
    let sumOfSquares limit =
        let xs = seq{ 1 .. 1 .. limit }
        Seq.fold( fun acc x -> acc + x * x ) 0 xs

    let squareOfSum limit =
        let xs = seq{ 1 .. 1 .. limit }
        let s = xs |> Seq.sum
        s * s

    let diff limit =
        (squareOfSum limit) - (sumOfSquares limit)

    let result = diff 100
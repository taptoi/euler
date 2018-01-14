namespace ProjectEuler

module P5 =
    // Brute force solution: 
    let smallestDivisibleByAll (xs: int list) =
        let nums = seq{ 20 .. 20 .. System.Int32.MaxValue }
        nums |> Seq.find(fun i -> 
                            xs |> List.forall(fun x -> i % x = 0))
    
    let input = [ 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    let solve = input |> smallestDivisibleByAll

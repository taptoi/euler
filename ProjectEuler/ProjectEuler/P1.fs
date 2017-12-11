namespace ProjectEuler

module P1 =

    let IsDividend x N = 
        x % N = 0. && x <> 0.

    let IsDividend3Or5 x =
        IsDividend x 3. || IsDividend x 5.

    let DividendsOf3Or5 N =
        let seqN = seq { 0. .. 1. .. (float(N)-1.) }
        seq { for x in seqN do 
                match IsDividend3Or5 x with
                | true -> yield int(x)
                | false -> ()
             }

    let result = DividendsOf3Or5 1000 |> Seq.sum

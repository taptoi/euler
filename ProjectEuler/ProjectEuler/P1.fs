namespace ProjectEuler

module P1 =
    let solve limit =
        {1 .. limit}
        |> Seq.filter (fun i -> i % 3 = 0 || i % 5 = 0)
        |> Seq.sum

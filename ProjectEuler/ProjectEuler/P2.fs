namespace ProjectEuler

    module P2 =
        let fibonacchi =
            let rec loop x0 x1 = seq { yield x1; yield! loop x1 (x0 + x1) }
            loop 1 2

        let limitedFibonacchi max =
            fibonacchi |> Seq.takeWhile(fun x -> x < max)

        let isEven x = x % 2 = 0

        let result =
            int(4e6) |> limitedFibonacchi |> Seq.where(fun x -> isEven(x)) |> Seq.sum


        


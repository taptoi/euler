namespace ProjectEuler

module P4 =

    let isPalindrome n = n.ToString().ToCharArray() |> Array.rev |> System.String.Concat |> n.ToString().Equals

    let findLargestPalindrome =
        let a = seq{ 100 .. 1 .. 999 }
        let s = seq{
                    for i in a do
                        for j in a do
                            let v = i * j
                            if (isPalindrome v) then yield v else ()
                }
        s |> Seq.max

    
    let x = findLargestPalindrome


//        let rec next ita itb =
//            let v =  ita * itb
//            match isPalindrome v with
//            | true -> yield v
//            | false -> if ita > minval then
//                            if itb > minval then next ita (itb - 1) else next (ita - 1) maxval
//                       else // ita 100
//                            if itb > minval then next ita (itb - 1) else ()
//        next maxval maxval
        
        
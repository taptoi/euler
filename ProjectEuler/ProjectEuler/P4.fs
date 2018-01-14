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

    
    let solve = findLargestPalindrome

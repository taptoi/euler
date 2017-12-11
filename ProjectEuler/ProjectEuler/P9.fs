module P9
    
    open System
    // Constraints:
    // 1. a < b < c
    // 2. a^2 + b^2 = c^2
    // 3. a + b + c = 1000

    //Algorithm:
    //1. Iterate a from 1 to 332 
    //2. There's a maximum limit for b, that is dependent on a:
    //    for example if a = 199, then b must be less than round((1000 - 199) / 2), so 400 then if c = 401 we get to 1000
    //3. In each a loop iterate for b with the limits from 2
    //4. In each b loop iterate for c with limits deducted from a + b + c = 1000

    let solve =
            seq{
                    for a in 1 .. 332 do
                        let minb = a + 1
                        let maxb = int(Math.Round((1000. - float(a)) / 2.)) - 1
                        for b in minb .. maxb do
                            let minc = b + 1
                            let maxc = 1000 - a - b
                            for c in minc .. maxc do
                                if ( a + b + c = 1000 ) && (a*a + b*b = c*c)
                                then yield (a * b * c)
                }
                |> Seq.head
      
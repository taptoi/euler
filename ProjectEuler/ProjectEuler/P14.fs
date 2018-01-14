module P14

let evenF n = n / 2L
let oddF n = 3L * n + 1L

let getNext (n : int64) : int64 =
    if n % 2L > 0L then oddF n else evenF n

let getChainLength input (cache : int64 array) (limit : int64) =
    // check if input has known length in cache
    // if yes, then return length + len_acc
    // if no then return yield a new input from lazySeq and increment length
    let rec chainLenRec (v : int64) (len_acc : int64) =
        if v = 1L then len_acc + 1L // exit by last item in chain
        elif (v < limit && cache.[int(v)] <> 0L) then cache.[int(v)] + len_acc // exit by cached value
        else chainLenRec (getNext v) (len_acc + 1L) // we need to traverse further
    chainLenRec input 0L

let solve' (limit : int64) =
    let cache = Array.init (int(limit) + 1) (fun _ -> 0L)
    let allChainLengths = seq{ 
                                for i in 2L .. limit do
                                    let len = getChainLength i cache limit
                                    cache.[int(i)] <- len
                                    yield (i, len)
                            }
    allChainLengths |> Seq.maxBy (fun item -> snd(item)) |> fst

let solve = solve' 1000000L


module P15

let E = (1,0)
let S = (0,1)

let tryMoveWith (curr : int*int) 
                (dir:int*int) 
                (limits:int*int) 
                (onMove : (int*int) -> unit) =
    let target = (fst(curr) + fst(dir), snd(curr) + snd(dir))
    if(fst(target) <= fst(limits) && snd (target) <= snd(limits))
    then onMove(target) else ignore()


let solve =
    let lim = 20
    let limits = (lim,lim)
    let mutable v = 0L
    let mutable count = 0L
    let mutable diagonals = Array.create<int64> lim 0L

    let rec moveNextIterative first (ds:int64[]) (curr : int*int)  =
        if fst(curr) = fst(limits) && snd(curr) = snd(limits) then
            count <- count + 1L
        elif fst(curr) = snd(curr) && ds.[fst(curr)] <> 0L then
            count <- count + ds.[fst(curr)]
        else
            let conditionalMove dir = 
                tryMoveWith curr dir limits (moveNextIterative false ds)
            if first then
                conditionalMove E
            else
                conditionalMove E
                conditionalMove S

    for i in 1 .. lim do
        let i' = lim - i
        moveNextIterative true diagonals (i', i')
        diagonals.[i'] <- count * 2L
        printf "diag %i" (count * 2L)
        v <- count * 2L
        count <- 0L
    v

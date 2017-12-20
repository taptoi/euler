module P18

open System
                            //75
                          //95 64
                        //17 47 82
                      //18 35 87 10
                    //20 04 82 47 65
                  //19 01 23 75 03 34
                //88 02 77 73 07 63 67
              //99 65 04 28 06 16 70 92
            //41 41 26 56 83 40 80 70 33
          //41 48 72 33 47 32 37 16 94 29
        //53 71 44 65 25 43 91 52 97 51 14
      //70 11 33 28 77 73 17 78 39 68 17 57
    //91 71 52 38 17 14 91 43 58 50 27 29 48
  //63 66 04 68 89 53 67 30 73 16 69 87 40 31
//04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

//3
//7 4
//2 4 6
//8 5 9 3

//That is, 3 + 7 + 4 + 9 = 23.


type Tree =
        | Tree of int * Tree * Tree
        | Leaf of int

let tree value leftSubTree rightSubTree =
    Tree (value, leftSubTree, rightSubTree)

let leaf value =
    Leaf (value)

let traverseAndFindMax =
    let mutable max = 0
    let rec next (current : Tree) (acc : int) =
        match current with
        | Leaf v -> 
            let candidate = v + acc
            if candidate > max then max <- candidate
        | Tree (v, left, right) -> 
            next left (acc + v)
            next right (acc + v)
    let leaves = [| Leaf 8; Leaf 5; Leaf 9; Leaf 3 |]
    let t2 = [| Tree (2, leaves.[0], leaves.[1]);  Tree (4, leaves.[1], leaves.[2]); Tree (6, leaves.[2], leaves.[3]); |]
    let t1 = [| Tree (7, t2.[0], t2.[1]);  Tree (4, t2.[1], t2.[2]); |]
    let t0 = Tree (3, t1.[0], t1.[1])
    next t0 0 |> ignore
    max




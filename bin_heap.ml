(* Binomial heap *)
module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module BinHeap (C : Comparable) = struct
  type tree = Node of int * C.t * tree list
  type heap = tree list

  let rank (Node (r, _, _)) = r

  let root (Node (_, x, _)) = x

  let link t1 t2 =
    let Node (r, x1, c1), Node (_, x2, c2) = t1, t2
    in if C.compare x1 x2 == 0
       then Node (r+1, x1, t2 :: c1)
       else Node (r+1, x2, t1 :: c2)

  let rec ins_tree t = function
    | [] -> [t]
    | (t'::ts') as ts -> if rank t < rank t'
                         then t :: ts
                         else ins_tree (link t t') ts'



end

(* Binomial heap *)
module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module BinHeap (C : Comparable) = struct
  type tree = Node of int * C.t * tree list
  type heap = tree list

  exception Empty

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

  let insert x t = ins_tree (Node (0, x, [])) t

(* To merge two heaps step through both lists of trees in increasing
   order of rank while linking trees of equal rank as we go *)
  let rec merge ts1 ts2 =
    match ts1, ts2 with
    | [], ts2 -> ts2
    | ts1, [] -> ts1
    | (t1::ts1'), (t2::ts2') -> if rank t1 < rank t2
                                then t1 :: merge ts1' ts2
                                else
                                  if rank t2 < rank t1
                                  then t2 :: merge ts1 ts2'
                                  else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree = function
    | [] -> raise Empty
    | [t] -> (t, [])
    | (t::ts) -> let (t', ts') = remove_min_tree ts
                 in if C.compare (root t) (root t') <= 0
                    then (t, ts)
                    else (t', t :: ts')

  let find_min ts = let (t, _) = remove_min_tree ts
                    in root t

  let delete_min ts = let (Node (_, x, ts1)), ts2 = remove_min_tree ts
                      in merge (List.rev ts1) ts2
end

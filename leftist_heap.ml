(* Leftist Heaps *)

module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module LeftistHeap (C : Comparable) = struct
  type heap =
    | Empty
    | Tree of int * C.t * heap * heap

  exception Empty_heap

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rank = function
    | Empty -> 0
    | Tree (r, _, _, _) -> r

  let makeTree x a b =
    if rank a >= rank b
    then Tree (rank b + 1, x, a, b)
    else Tree (rank a + 1, x, b, a)

  let rec merge x y =
    match x, y with
    | Empty, _ -> y
    | _, Empty -> x
    | Tree (_, a, l1, r1), Tree (_, b, l2, r2) ->
       match C.compare a b with
       | n when n <= 0 -> makeTree a l1 (merge r1 y)
       | _ -> makeTree b l2 (merge x r2)

  let insert x h = merge (Tree (1, x, Empty, Empty)) h

  let find_min = function
    | Empty -> raise Empty_heap
    | Tree (_, x, _, _) -> x

  let delete_min = function
    | Empty -> raise Empty_heap
    | Tree (_, x, l, r) -> merge l r
end

(* Comparable functor instance for int *)
module IntC = struct
  type t = int
  let compare = (-)
end

module LeftistIntHeap = LeftistHeap(IntC)

let tests =
  let open LeftistIntHeap in
  let h = empty in
  let h = insert 1 h in
  let h = insert 5 h in
  let h = insert 3 h in
  h

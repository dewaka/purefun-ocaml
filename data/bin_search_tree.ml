(* Binary Search Trees *)

module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module BinSTree (C : Comparable) = struct
  type tree =
    | Empty
    | Tree of tree * C.t * tree

  let rec insert tr x =
    match tr with
    | Empty -> Tree (Empty, x, Empty)
    | Tree (left, v, right) -> match C.compare x v with
                               | n when n <= 0 -> Tree (insert left x, v, right)
                               | _ -> Tree (left, v, insert right x)

  let rec member tr x =
    match tr with
    | Empty -> false
    | Tree (left, v, right) -> match C.compare x v with
                               | n when n < 0 -> member left x
                               | n when n > 0 -> member right x
                               | _ -> true (* n==0 case *)
end

(* Comparable Functor instance for int *)
module IntC = struct
  type t = int
  let compare = (-)
end

module BinIntTree = BinSTree(IntC)

(* Test instances *)
let tests =
  let open BinIntTree in
  let tr = insert Empty 1 in
      let tr = insert tr 3 in
      let tr = insert tr 10 in
      assert (member tr 3)

(* Red Black Trees *)

(* Invariants of Red Black Trees
 * No red node has a red child
 * Every path from the root to an empty node contains the same number of
   black nodes
*)

module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module RedBlackTree (C : Comparable) = struct
  type color = R | B
  type tree = E | T of color * tree * C.t * tree

  let rec member x t =
    match t with
    | E -> false
    | T (_, l, y, r) -> match C.compare x y with
                        | n when n < 0 -> member x l
                        | n when n > 0 -> member x r
                        | _ -> true

  let rec balance color left elem right =
    match color, left, elem, right with
    | (B, T (R, T (R, a, x, b), y, c), z, d)
    | (B, T (R, a, x, T (R, b, y, c)), z, d)
    | (B, a, x, T (R, T (R, b, y, c), z, d))
    | (B, a, x, T (R, b, y, T (R, c, z, d)))
      -> T (R, T (B, a, x, b), y, T (B, c, z, d))
    | _ -> T (color, left, elem, right)

  let insert x t =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, left, y, right) as s ->
         if x < y then balance color (ins left) y right
         else if x > y then balance color left y (ins right)
         else s
    in
    let T (_, a, y, b) = ins t
    in T (B, a, y, b)

end

(* Test instances *)
module IntC = struct
  type t = int
  let compare = (-)
end

module IntRedBlackTree = RedBlackTree(IntC)

let test =
  let open IntRedBlackTree in
  let h = insert 7 E in
  let h = insert 3 h in
  let h = insert 13 h in
  h

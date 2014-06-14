(* String Processing Algorithms *)

module Strings = struct

  let rec is_prefix xs ys =
    match xs, ys with
    | [], _ -> true
    | _, [] -> false
    | (x::xs'), (y::ys') -> x=y && is_prefix xs' ys'

  let rec sub_list xs ys =
    match ys with
    | [] -> false
    | (_::ys') -> is_prefix xs ys || sub_list xs ys'

  let explode str =
    let n = String.length str in
    let rec go i =
      if i < n then String.get str i :: go (i+1)
      else []
    in go 0

  let starts_with s str = is_prefix (explode s) (explode str)

  let substring s str = sub_list (explode s) (explode str)

  (* Brute force algorithm for substring search *)
  let simple_search s str =
    let m, n = String.length s, String.length str in
    let rec prefix i j =
      if i >= m then Some (j-m)
      else if j-i > n-m then None
      else if (String.get s i) = (String.get str j) then prefix (i+1) (j+1)
      else None
    in
    let rec search j =
      if j >= n then None
      else match prefix 0 j with
           | None -> search (j+1)
           | m -> m
    in search 0

  (* Knuth-Morris-Pratt String matching algorithm *)
  let kmp_match needle haystack =
    let m, n = String.length needle, String.length haystack in
    let rec search i j =
      if i >= m then Some (((j-m)))
      else if j-i > n-m then None
      else if (String.get needle i) = (String.get haystack j) then search (i+1) (j+1)
      else search 0 (j+1)
    in search 0 0

end

(* String Processing Algorithms *)

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

let simple_search s str =
  let m, n = String.length s, String.length str in
  let rec prefix i j =
    if i >= m then true
    else if j >= n then false
    else (String.get s i) = (String.get str j) && prefix (i+1) (j+1)
  in
  let rec search j =
    if j >= n then false
    else prefix 0 j || search (j+1)
  in search 0

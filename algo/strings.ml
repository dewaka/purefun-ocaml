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

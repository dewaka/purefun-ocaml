(* Helper operators *)
let (--) m n =
  let rec aux i = if i <= n then i::aux (i+1) else []
  in aux m

(* Pipe operators *)
let (|>) x f = f x
let (<|) f x = f x

let flip f x y = f y x

(* String Processing Algorithms *)
module Strings = struct
  (* Helper function to lookup values of assoc lists *)
  let rec lookup x xs =
    match xs with
    | [] -> None
    | ((k,v)::xs') -> if k=x then Some v else lookup x xs'

  (* Helper function to update or insert values of assoc lists *)
  let rec upsert (k, v) xs =
    match xs with
    | [] -> [(k, v)]
    | ((k',v')::xs') -> if k=k' then (k, v)::xs' else (k',v')::upsert (k, v) xs'

  let rec list_suffixes = function
      | [] -> []
      | (_::xs) as ls -> ls :: list_suffixes xs

  let insert_missing (k, v) xs =
    match lookup k xs with
    | None -> (k, v)::xs
    | Some (_) -> xs

  let rec foldl f a xs =
    match xs with
    | [] -> a
    | (x::xs') -> foldl f (f a x) xs'

  let rec zip xs ys =
    match xs, ys with
    | (x::xs'), (y::ys') -> (x,y)::zip xs' ys'
    | _, _ -> []

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
      else
        if (String.get needle i) = (String.get haystack j)
        then search (i+1) (j+1)
        else search 0 (j+1)
    in search 0 0

  let partial_match_table str =
    let n = String.length str in
    let ls = explode str in
    foldl (flip insert_missing) [] (zip ls (0 -- (n-1)))

  let suffixes str =
    let ls = explode str
    in list_suffixes ls

  let prefixes str =
    let ls = List.rev (explode str)
    in List.rev (List.map List.rev (list_suffixes ls))

  let match_count str =
    let ps, ss = prefixes str, suffixes str in
    let sum = foldl (+) 0 in
    let count_matches ls x = sum (List.map (fun e -> if x=e then 1 else 0) ls) in
    let proper_matches = sum (List.map (count_matches ps) ss) - 1 in
    proper_matches

  let proper_prefixes str =
    let sx = ref [] in
    for i = 1 to (String.length str - 1) do
      sx := String.sub str 0 i::!sx;
    done;
    !sx

  let proper_suffixes str =
    let sx = ref [] in
    let n = String.length str in
    for i = n-1 downto 1 do
      sx := String.sub str i (n-i)::!sx;
    done;
    !sx

  (* To find the KMP index I have to find the length of the biggest common
     substring in proper prefixes and proper suffixes *)
  let kmp_index str =
    let ps, ss = proper_prefixes str, proper_suffixes str in
    let rec contains elem = function
      | [] -> false
      | (x::xs) -> x=elem || contains elem xs in
    let rec index = function
      | [] -> 0
      | (x::xs) -> if contains x ps then String.length x
                   else index xs
    in index ss

  let kms_index_array str =
    let n = String.length str in
    let arr : int array = Array.make n 0 in
    for i = 0 to n-1 do
      let s = String.sub str 0 (i+1) in
      let k = kmp_index s in
      Array.set arr i k
    done;
    arr

  let print_partial_match_table str =
    let n = String.length str in
    let substrs = List.map (String.sub str 0) (1 -- n) in
    List.map match_count substrs

end

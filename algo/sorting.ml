(* Helper functions *)

let rec filter p xs =
  match xs with
  | [] -> []
  | (x::xs') -> if p x then x::(filter p xs')
                else filter p xs'

let rec take n xs =
  match n, xs with
  | _, [] -> []
  | n, _ when n <=0 -> []
  | n, (x::xs') -> x::take (n-1) xs'

let rec drop n xs =
  match n, xs with
  | _, [] -> []
  | n, xs when n <= 0 -> xs
  | n, (_::xs') -> drop (n-1) xs'

let length ls =
  let rec tlen acc = function
  | [] -> acc
  | (_::xs) -> tlen (acc+1) xs
  in tlen 0 ls

let split_at n ls =
  let rec loop n fs rs =
    match n, rs with
    | n, _ when n <= 0 -> fs, rs
    | n, [] -> raise (Failure "List is smaller than split position")
    | n, (x::xs) -> loop (n-1) (fs @ [x]) xs
  in loop n [] ls

let print_list ls =
  let open Printf in
  let rec loop = function
    | [] -> printf "]\n"
    | [x] -> printf "%d]\n" x
    | (x::xs) -> printf "%d; " x; loop xs in
  printf "["; loop ls

let (--) m n =
  let rec go m n = if m <= n then m::go (m+1) n
                   else []
  in go m n
        
let reverse ls = 
  let rec loop acc ls =
    match ls with
    | [] -> acc
    | (x::xs) -> loop (x::acc) xs
  in loop [] ls

(* Quicksort *)
let rec quick_sort = function
  | [] -> []
  | (x::xs) -> let smaller = filter (fun p -> p < x) xs in
               let larger = filter (fun p -> p >= x) xs in
               quick_sort smaller @ [x] @ quick_sort larger

(* Merge sort *)


let rec merge_sort ls =
  let rec merge xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | (x::xs'), (y::ys') -> if x < y then x::merge xs' ys
                            else y::merge xs ys'
  in
  match ls with
  | [] -> []
  | [x] -> [x]
  | ls ->
     let first_half, second_half = split_at (length ls / 2) ls in
     merge (merge_sort first_half) (merge_sort second_half)

(* Insertion sort *)
let rec insertion_sort xs =
  let rec insert x ls =
    match ls with
    | [] -> [x]
    | (y::ys) -> if x < y then x::ls else y::insert x ys
  in
  match xs with
  | [] -> []
  | (x::xs') -> insert x (insertion_sort xs')

let bubble_sort lst = 
  let rec sort accum rev lst =
    match lst, rev with
    | [], true -> reverse accum 
    | [], false -> sort [] true (reverse accum)
    | x::y::tail, _ when x > y -> sort (y::accum) false (x::tail)
    | head::tail, _ -> sort (head::accum) rev tail
  in sort [] true lst

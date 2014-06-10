(* Helper functions *)

let rec filter p xs =
  match xs with
  | [] -> []
  | (x::xs') -> if p x then x::(filter p xs')
                else filter p xs'

(* Quicksort *)
let rec quick_sort = function
  | [] -> []
  | (x::xs) -> let smaller = filter (fun p -> p < x) xs in
               let larger = filter (fun p -> p >= x) xs in
               quick_sort smaller @ [x] @ quick_sort larger

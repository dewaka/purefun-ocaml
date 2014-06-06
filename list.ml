module List = struct
  type 'a list =
    | Nil
    | Cons of 'a * ('a list)

  (* Tail recursive length function *)
  let length xs =
    let rec tlen n xs =
      match xs with
      | Nil -> n
      | Cons (_, ls) -> tlen (n+1) ls
    in tlen 0 xs

  let rec append xs ys =
    match xs, ys with
    | Nil, _ -> ys
    | _, Nil -> xs
    | Cons (x, xs'), _  -> Cons (x, append xs' ys)

  let rec reverse = function
    | Nil -> Nil
    | Cons (x, xs') -> append (reverse xs') (Cons (x, Nil))

  let rec map f = function
    | Nil -> Nil
    | Cons (x, xs') -> Cons (f x, map f xs')

  let rec foldl f a xs =
    match xs with
    | Nil -> a
    | Cons (x, xs') -> foldl f (f a x) xs'

  let sum xs =
    foldl (fun x y -> x + y) 0 xs

  let product xs =
    foldl (fun x y -> x * y) 1 xs

  (* range generates a list with range of numbers such that x in the range
   is m <= x <= n holds. i is the optional increment which defaults to 1 *)
  let rec range ?(i=1) m n =
    match m, n with
    | m, n when m <= n -> Cons (m, range ~i:i (m+i) n)
    | _, _ -> Nil

end

open List
let sample_x = Cons (1, Cons (2, Nil))
let sample_y = range 1 10

let sample_y_sum = sum sample_y

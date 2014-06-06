module List = struct
  type 'a list = 
    | Nil
    | Cons of 'a * ('a list)

  let rec len = function
    |  Nil -> 0
    | Cons (_, ls) -> 1 + len ls
                             
  let rec append xs ys =
    match xs, ys with
    | Nil, _ -> ys
    | _, Nil -> xs
    | Cons (x, xs'), _  -> Cons (x, append xs' ys)
                                
  let rec reverse = function
    | Nil -> Nil
    | Cons (x, xs') -> append (reverse xs') (Cons (x, Nil))
end

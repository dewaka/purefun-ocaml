module Stack = struct
  open List

  type 'a stack = 'a list
  exception Empty_stack

  let empty_stack = Nil

  let push st x = Cons (x, st)

  let pop = function
    | Nil -> raise Empty_stack
    | Cons (x, xs) -> x, xs

  let peek = function
    | Nil -> raise Empty_stack
    | Cons (x, _) -> x

end

(* Test Stack *)
let tests =
  let open Stack in
  let x = push empty_stack 10 in
  let x = push x 12 in
  let x = push x 13 in
  let x = push x 14 in
  let (p, _) = pop x
  in assert (p == 14)

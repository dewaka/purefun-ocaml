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
open Stack

let x = push empty_stack 10
let x = push x 12
let x = push x 13
let x = push x 14

type t =
  | Variable of string
  | Function of string * t list

(* A horribly lazy, fragile hack to get things running *)
let new_eigenvariable =
  let number = Random.int 1_000_000 in
  let name = string_of_int number in
  Variable(name)

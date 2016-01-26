(* A good example of use case where terms are not really needed, but what to
   do with the structure? -> Now extended *)
type t =
| H of t
| A
| Variable of string
| Dummy

let new_eigenvariable =
  let number = Random.int 1_000_000 in
  let name = string_of_int number in
  Variable(name)

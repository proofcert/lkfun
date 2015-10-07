type t =
  | Variable of string
  | Function of string * t list

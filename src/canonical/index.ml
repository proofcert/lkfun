type t =
| Lit
| Tlit
| Idx of int
| Pid of int * t

let compare (x : t) y =
  Pervasives.compare x y

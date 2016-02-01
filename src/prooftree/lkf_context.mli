type t

exception Nonfunctional

val empty : t

val add : Index.t -> Lkf_formula.t -> t -> t

val find : Index.t -> t -> Lkf_formula.t option

type t =
  | PositiveTrue
  | NegativeTrue
  | PositiveFalse
  | NegativeFalse
  | PositiveAtom of Atom.t
  | NegativeAtom of Atom.t
  | PositiveAnd of t * t
  | NegativeAnd of t * t
  | PositiveOr of t * t
  | NegativeOr of t * t
  | ForAll of (Term.t -> t)
  | Exists of (Term.t -> t)

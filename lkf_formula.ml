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

let rec neg = function
  | NegativeFalse -> PositiveTrue
  | PositiveTrue -> NegativeFalse
  | NegativeTrue -> PositiveFalse
  | PositiveFalse -> NegativeTrue
  | PositiveAtom(atom) -> NegativeAtom(atom)
  | NegativeAtom(atom) -> PositiveAtom(atom)
  | PositiveAnd(left, right) -> NegativeOr(neg left, neg right)
  | NegativeOr(left, right) -> PositiveAnd(neg left, neg right)
  | NegativeAnd(left, right) -> PositiveOr(neg left, neg right)
  | PositiveOr(left, right) -> NegativeAnd(neg left, neg right)
  | ForAll(generator) -> Exists(fun x -> neg (generator x))
  | Exists(generator) -> ForAll(fun x -> neg (generator x))

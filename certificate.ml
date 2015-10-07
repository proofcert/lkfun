type t =
  | FalseClerk of t
  | AndClerk of t * t
  | OrClerk of t
  | AllClerk of (Term.t -> t)
  | StoreClerk of t * Index.t
  | TrueExpert
  | AndExpert of t * t
  | OrExpert of t * Choice.t
  | ExistsExpert of t * Term.t
  | CutExpert of t * t * Lkf_formula.t
  | InitExpert of Index.t
  | ReleaseExpert of t
  | DecideExpert of t * Index.t

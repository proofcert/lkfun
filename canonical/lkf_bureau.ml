open Certificate

(* Quick and dirty, using catch-alls for exhaustive matches *)
(* Again, this does not comprimise soundness, so it is kinda sorta just OK *)

let false_clerk = function
  | FalseClerk(next) -> Some next
  | _ -> None

let and_clerk = function
  | AndClerk(left, right) -> Some (left, right)
  | _ -> None

let or_clerk = function
  | OrClerk(next) -> Some next
  | _ -> None

let all_clerk = function
  | AllClerk(next) -> Some next
  | _ -> None

let store_clerk = function
  | StoreClerk(next, index) -> Some (next, index)
  | _ -> None

let true_expert = function
  | TrueExpert -> true
  | _ -> false

let and_expert = function
  | AndExpert(left, right) -> Some (left, right)
  | _ -> None

let or_expert = function
  | OrExpert(next, choice) -> Some (next, choice)
  | _ -> None

let exists_expert = function
  | ExistsExpert(next, term) -> Some (next, term)
  | _ -> None

let cut_expert = function
  | CutExpert(positive, negative, formula) -> Some (positive, negative, formula)
  | _ -> None

let init_expert = function
  | InitExpert(index) -> Some index
  | _ -> None

let release_expert = function
  | ReleaseExpert(next) -> Some next
  | _ -> None

let decide_expert = function
  | DecideExpert(next, index) -> Some (next, index)
  | _ -> None

open Certificate

let false_clerk = function
  | Singleton(next) -> Some next
  | _ -> None

let and_clerk = function
  | And(left, right) -> Some (left, right)
  | _ -> None

let or_clerk = function
  | Singleton(next) -> Some next
  | _ -> None

let all_clerk = function
  | Forall(next) -> Some next
  | _ -> None

let store_clerk = function
  | Index(index, next) -> Some (next, index)
  | _ -> None

let true_expert = function
  | End -> true
  | _ -> false

let and_expert = function
  | And(left, right) -> Some (left, right)
  | _ -> None

let or_expert = function
  | OrPositive(choice, next) -> Some (next, choice)
  | _ -> None

let exists_expert = function
  | Exists(term, next) -> Some (next, term)
  | _ -> None

let cut_expert = function
  | Cut(formula, positive, negative) -> Some (positive, negative, formula)
  | _ -> None

let init_expert = function
  | Initial(index) -> Some index
  | _ -> None

let release_expert = function
  | Singleton(next) -> Some next
  | _ -> None

let decide_expert = function
  | Index(index, next) -> Some (next, index)
  | _ -> None

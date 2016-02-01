module Indexed = struct
  type t = Index.t
  let compare i1 i2 =
    Index.compare i1 i2
end

module IndexMap = Map.Make(Indexed)

type t = Lkf_formula.t IndexMap.t

exception Nonfunctional

let empty =
  IndexMap.empty

let add index formula map =
  if IndexMap.mem index map then
    raise Nonfunctional
  else
    IndexMap.add index formula map

let find index map =
  try
    let formula = IndexMap.find index map in
    Some formula
  with
    Not_found -> None

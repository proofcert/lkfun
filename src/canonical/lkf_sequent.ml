type t =
  | Unfocused of Lkf_context.t * Lkf_formula.t list (* List-as-ordered-multiset *)
  | Focused of Lkf_context.t * Lkf_formula.t

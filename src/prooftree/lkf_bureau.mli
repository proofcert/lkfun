val false_clerk : Certificate.t -> Certificate.t option

val and_clerk : Certificate.t -> (Certificate.t * Certificate.t) option

val or_clerk : Certificate.t -> Certificate.t option

val all_clerk : Certificate.t -> (Term.t -> Certificate.t) option

val store_clerk : Certificate.t -> (Certificate.t * Index.t) option

val true_expert : Certificate.t -> bool

val and_expert : Certificate.t -> (Certificate.t * Certificate.t) option

val or_expert : Certificate.t -> (Certificate.t * Choice.t) option

val exists_expert : Certificate.t -> (Certificate.t * Term.t) option

val cut_expert : Certificate.t -> (Certificate.t * Certificate.t * Lkf_formula.t) option

val init_expert : Certificate.t -> Index.t option

val release_expert : Certificate.t -> Certificate.t option

val decide_expert : Certificate.t -> (Certificate.t * Index.t) option

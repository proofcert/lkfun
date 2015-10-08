open Lkf_formula
open Lkf_sequent
open Choice

(* This looks repetitive in many ways... exceptions?
   Moreover, the mapping is fixed, shouldn't this come from outside?
   Currently cut-free! *)
let rec check certificate = function
  | Unfocused(storage, workbench) ->
    (match workbench with
    | [] ->
      decide_expert certificate storage
    | hd :: tl ->
      (match hd with
      | NegativeFalse ->
        false_clerk certificate storage tl
      | NegativeAnd(left, right) ->
        and_clerk left right certificate storage tl
      | NegativeOr(left, right) ->
        or_clerk left right certificate storage tl
      | NegativeTrue ->
        true
      | ForAll(formula) ->
        forall_clerk formula certificate storage tl
      | NegativeAtom(_) | PositiveAnd(_,_) | PositiveOr(_,_) | Exists(_) | PositiveAtom(_) | PositiveTrue | PositiveFalse ->
        store_clerk hd certificate storage tl
      )
    )
  | Focused(storage, workbench) ->
    (match workbench with
    | PositiveTrue ->
      true_expert certificate
    | PositiveAnd(left, right) ->
      and_expert left right certificate storage
    | PositiveOr(left, right) ->
      or_expert left right certificate storage
    | Exists(formula) ->
      exists_expert formula certificate storage
    | PositiveAtom(atom) ->
      init_expert atom certificate storage
    | PositiveFalse ->
      false
    | NegativeAnd(_,_) | NegativeOr(_,_) | ForAll(_) | NegativeAtom(_) | NegativeTrue | NegativeFalse ->
      release_expert workbench certificate storage
    )

and false_clerk certificate storage workbench =
  match Lkf_bureau.false_clerk certificate with
    | None -> false
    | Some certificate' ->
      check certificate' (Unfocused(storage, workbench))

and and_clerk left right certificate storage workbench =
  match Lkf_bureau.and_clerk certificate with
    | None -> false
    | Some (certificate_left, certificate_right) ->
      let check_left = check certificate_left (Unfocused(storage, left :: workbench))
      and check_right = check certificate_right (Unfocused(storage, right :: workbench)) in
      check_left && check_right

and or_clerk left right certificate storage workbench =
  match Lkf_bureau.or_clerk certificate with
    | None -> false
    | Some certificate' ->
      check certificate' (Unfocused(storage, left :: right :: workbench))

and forall_clerk formula certificate storage workbench =
  match Lkf_bureau.all_clerk certificate with
    | None -> false
    | Some certificate' ->
      let eigenvariable = Term.new_eigenvariable in
      let certificate_filled = certificate' eigenvariable
      and formula_filled = formula eigenvariable in
      check certificate_filled (Unfocused(storage, formula_filled :: workbench))

(* Enforce invariant on indexes? *)
and store_clerk formula certificate storage workbench =
  match Lkf_bureau.store_clerk certificate with
    | None -> false
    | Some (certificate', index) ->
      let storage' = Lkf_context.add index formula storage in
      check certificate' (Unfocused(storage', workbench))

and true_expert certificate =
  Lkf_bureau.true_expert certificate

and and_expert left right certificate storage =
  match Lkf_bureau.and_expert certificate with
    | None -> false
    | Some (certificate_left, certificate_right) ->
      let check_left = check certificate_left (Focused(storage, left))
      and check_right = check certificate_right (Focused(storage, right)) in
      check_left && check_right

and or_expert left right certificate storage =
  match Lkf_bureau.or_expert certificate with
    | None -> false
    | Some (certificate', choice) ->
      (match choice with
      | Left ->
        check certificate' (Focused(storage, left))
      | Right ->
        check certificate' (Focused(storage, right))
      )

and exists_expert formula certificate storage =
  match Lkf_bureau.exists_expert certificate with
    | None -> false
    | Some (certificate', term) ->
      let formula_filled = formula term in
      check certificate' (Focused(storage, formula_filled))

and init_expert atom certificate storage =
  match Lkf_bureau.init_expert certificate with
    | None -> false
    | Some index ->
      (match Lkf_context.find index storage with
      | None -> false
      | Some formula ->
        formula = NegativeAtom(atom)
      )

and release_expert formula certificate storage =
  match Lkf_bureau.release_expert certificate with
    | None -> false
    | Some certificate' ->
      check certificate' (Unfocused(storage, [formula]))

(* Here and elsewhere, probably move the matching somewhere else;
  abstract away the polarity check. *)
and decide_expert certificate storage =
  match Lkf_bureau.decide_expert certificate with
    | None -> false
    | Some (certificate', index) ->
      (match Lkf_context.find index storage with
      | None -> false
      | Some formula ->
        (match formula with
        | PositiveTrue | PositiveAnd(_,_) | PositiveOr(_,_) | Exists(_) | PositiveAtom(_) | PositiveFalse ->
          check certificate' (Focused(storage, formula))
        | NegativeFalse | NegativeAnd(_,_) | NegativeOr(_,_) | ForAll(_) | NegativeAtom(_) | NegativeTrue -> false (* Exception? This shouldn't happen *)
        )
      )

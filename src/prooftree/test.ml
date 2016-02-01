(* ocamlbuild -use-ocamlfind -pkg oUnit test.byte *)
open OUnit2

let test_integration_01 _ =
  assert_equal true
    (Lkf_kernel.check
      (AllClerk(fun _ -> OrClerk(StoreClerk(StoreClerk(DecideExpert(
        InitExpert(2), 1), 2), 1))))
      (Unfocused(Lkf_context.empty,
        [ForAll (fun x -> NegativeOr(
          PositiveAtom(Relation("p", [x])), NegativeAtom(Relation("p", [x]))))])))

let suite = "OUnit2 test suite" >::: ["test_integration_01" >:: test_integration_01]

let _ =
  run_test_tt_main suite

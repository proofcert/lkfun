(* ocamlbuild -use-ocamlfind -pkg oUnit test.byte *)
open OUnit2

open Lkf_formula
open Certificate

let test_integration_01 _ =
  let formula =
    NegativeOr(
      PositiveAnd(NegativeAtom(R1), NegativeAtom(R2)),
    NegativeOr(
      PositiveAnd(PositiveAtom(R1), NegativeAtom(R2)),
      PositiveAtom(R2)))
  and certificate =
Singleton(
Index(Pid(0, Idx(1)),
Singleton(
Index(Pid(1, (Idx(2))),
Index(Pid(2, (Idx(3))),
Cut(PositiveAtom(R2),
	Index(Pid(3, Lit),
	Index(Pid(0, Idx(1)),
	And(
		Singleton(
		Index(Pid(4, Tlit),
		Index(Pid(1, Idx(2)),
		And(
			Initial(Pid(4, Tlit))
			,
			Singleton(
			Index(Pid(5, Lit),
			Index(Pid(3, Lit),
			Initial(Pid(5, Lit))
			)))
		))))
		,
		Singleton(
		Index(Pid(4, Lit),
		Index(Pid(3, Lit),
		Initial(Pid(4, Lit))
		)))
	)))
	,
	Index(Pid(3, Idx(4)),
	Cut(NegativeFalse,
		Singleton(
		Index(Pid(2, Idx(3)),
		Initial(Pid(3, Idx(4)))
		))
		,
		Index(Pid(4, Tlit),
		Index(Pid(4, Tlit),
		End
		))
	))
))))))
  in
  assert_equal true
    (Lkf_kernel.check certificate (Unfocused(Lkf_context.empty, [formula])))

let test_integration_02 _ =
  let formula =
    NegativeOr(
      NegativeAtom(G(A)),
      NegativeOr(
        PositiveAtom(G(H(H(A)))),
        Exists(fun x -> PositiveAnd(PositiveAtom(G(x)), NegativeAtom(G(H(x)))))))
  and certificate =
Singleton(
Index(Pid(0, Idx(1)),
Singleton(
Index(Pid(1, Idx(2)),
Index(Pid(2, Idx(3)),
Cut(PositiveAtom(G(H(A))),
	Index(Pid(3, Lit),
	Index(Pid(2, Idx(3)),
	Exists(A,
	And(
		Initial(Pid(0, Idx(1)))
		,
		Singleton(
		Index(Pid(4, Lit),
		Index(Pid(3, Lit),
		Initial(Pid(4, Lit)))))
	))))
	,
	Index(Pid(3, Idx(4)),
	Cut(PositiveAtom(G(H(H(A)))),
		Index(Pid(4, Lit),
		Index(Pid(2, Idx(3)),
		Exists(H(A),
		And(
			Initial(Pid(3, Idx(4)))
			,
			Singleton(
			Index(Pid(5, Lit),
			Index(Pid(4, Lit),
			Initial(Pid(5, Lit)))))
		))))
		,
		Index(Pid(4, Idx(5)),
		Cut(NegativeFalse,
			Singleton(
			Index(Pid(1, Idx(2)),
			Initial(Pid(4, Idx(5)))))
			,
			Index(Pid(5, Tlit),
			Index(Pid(5, Tlit),
			End))
		))
	))
))))))
  in
  assert_equal true
    (Lkf_kernel.check certificate (Unfocused(Lkf_context.empty, [formula])))

let suite = "OUnit2 test suite" >:::
  ["test_integration_01" >:: test_integration_01;
   "test_integration_02" >:: test_integration_02]

let _ =
  run_test_tt_main suite

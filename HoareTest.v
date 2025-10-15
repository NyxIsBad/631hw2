Set Warnings "-notation-overridden,-parsing".
From Stdlib Require Export String.
From PLF Require Import Hoare.

Parameter MISSING: Type.

Module Check.

Ltac check_type A B :=
    match type of A with
    | context[MISSING] => idtac "Missing:" A
    | ?T => first [unify T B; idtac "Type: ok" | idtac "Type: wrong - should be (" B ")"]
    end.

Ltac print_manual_grade A :=
    match eval compute in A with
    | Some (_ ?S ?C) =>
        idtac "Score:"  S;
        match eval compute in C with
          | ""%string => idtac "Comment: None"
          | _ => idtac "Comment:" C
        end
    | None =>
        idtac "Score: Ungraded";
        idtac "Comment: None"
    end.

End Check.

From PLF Require Import Hoare.
Import Check.

Goal True.

idtac "-------------------  hoare_pre_false  --------------------".
idtac " ".

idtac "#> hoare_pre_false".
idtac "Possible points: 1".
check_type @hoare_pre_false (
(forall (P Q : Assertion) (c : com) (_ : forall st : state, not (P st)),
 valid_hoare_triple P c Q)).
idtac "Assumptions:".
Abort.
Print Assumptions hoare_pre_false.
Goal True.
idtac " ".

idtac "-------------------  hoare_asgn_examples_2  --------------------".
idtac " ".

idtac "#> assertion_sub_ex1'".
idtac "Possible points: 1".
check_type @assertion_sub_ex1' (
(valid_hoare_triple
   (fun st : state =>
    le ((Aexp_of_aexp (AId X) : Aexp) st) ((Aexp_of_nat 5 : Aexp) st))
   (CAsgn X (AMult (ANum 2) (AId X)))
   (fun st : state =>
    le ((Aexp_of_aexp (AId X) : Aexp) st) ((Aexp_of_nat 10 : Aexp) st)))).
idtac "Assumptions:".
Abort.
Print Assumptions assertion_sub_ex1'.
Goal True.
idtac " ".

idtac "#> assertion_sub_ex2'".
idtac "Possible points: 1".
check_type @assertion_sub_ex2' (
(valid_hoare_triple
   (fun st : state =>
    and
      (((fun st0 : state =>
         le ((Aexp_of_nat 0 : Aexp) st0) ((Aexp_of_nat 3 : Aexp) st0))
        :
        Assertion) st)
      (((fun st0 : state =>
         le ((Aexp_of_nat 3 : Aexp) st0) ((Aexp_of_nat 5 : Aexp) st0))
        :
        Assertion) st))
   (CAsgn X (ANum 3))
   (fun st : state =>
    and
      (((fun st0 : state =>
         le ((Aexp_of_nat 0 : Aexp) st0) ((Aexp_of_aexp (AId X) : Aexp) st0))
        :
        Assertion) st)
      (((fun st0 : state =>
         le ((Aexp_of_aexp (AId X) : Aexp) st0) ((Aexp_of_nat 5 : Aexp) st0))
        :
        Assertion) st)))).
idtac "Assumptions:".
Abort.
Print Assumptions assertion_sub_ex2'.
Goal True.
idtac " ".

idtac "-------------------  if_minus_plus  --------------------".
idtac " ".

idtac "#> if_minus_plus".
idtac "Possible points: 2".
check_type @if_minus_plus (
(valid_hoare_triple (fun _ : state => True)
   (CIf (BLe (AId X) (AId Y)) (CAsgn Z (AMinus (AId Y) (AId X)))
      (CAsgn Y (APlus (AId X) (AId Z))))
   (fun st : state =>
    @eq nat ((Aexp_of_aexp (AId Y) : Aexp) st)
      (((fun st0 : state =>
         PeanoNat.Nat.add ((Aexp_of_aexp (AId X) : Aexp) st0)
           ((Aexp_of_aexp (AId Z) : Aexp) st0))
        :
        Aexp) st)))).
idtac "Assumptions:".
Abort.
Print Assumptions if_minus_plus.
Goal True.
idtac " ".

idtac " ".

idtac "Max points - standard: 5".
idtac "Max points - advanced: 5".
idtac "".
idtac "Allowed Axioms:".
idtac "functional_extensionality".
idtac "FunctionalExtensionality.functional_extensionality_dep".
idtac "CSeq_congruence".
idtac "fold_constants_bexp_sound".
idtac "succ_hastype_nat__hastype_nat".
idtac "".
idtac "".
idtac "********** Summary **********".
idtac "".
idtac "Below is a summary of the automatically graded exercises that are incomplete.".
idtac "".
idtac "The output for each exercise can be any of the following:".
idtac "  - 'Closed under the global context', if it is complete".
idtac "  - 'MANUAL', if it is manually graded".
idtac "  - A list of pending axioms, containing unproven assumptions. In this case".
idtac "    the exercise is considered complete, if the axioms are all allowed.".
idtac "".
idtac "********** Standard **********".
idtac "---------- hoare_pre_false ---------".
Print Assumptions hoare_pre_false.
idtac "---------- assertion_sub_ex1' ---------".
Print Assumptions assertion_sub_ex1'.
idtac "---------- assertion_sub_ex2' ---------".
Print Assumptions assertion_sub_ex2'.
idtac "---------- if_minus_plus ---------".
Print Assumptions if_minus_plus.
idtac "".
idtac "********** Advanced **********".
Abort.

(* 2025-10-10 12:12 *)

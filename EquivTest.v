Set Warnings "-notation-overridden,-parsing".
From Stdlib Require Export String.
From PLF Require Import Equiv.

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

From PLF Require Import Equiv.
Import Check.

Goal True.

idtac "-------------------  skip_right  --------------------".
idtac " ".

idtac "#> skip_right".
idtac "Possible points: 2".
check_type @skip_right ((forall c : com, cequiv (CSeq c CSkip) c)).
idtac "Assumptions:".
Abort.
Print Assumptions skip_right.
Goal True.
idtac " ".

idtac "-------------------  if_false  --------------------".
idtac " ".

idtac "#> if_false".
idtac "Possible points: 2".
check_type @if_false (
(forall (b : bexp) (c1 c2 : com) (_ : bequiv b BFalse),
 cequiv (CIf b c1 c2) c2)).
idtac "Assumptions:".
Abort.
Print Assumptions if_false.
Goal True.
idtac " ".

idtac "-------------------  swap_if_branches  --------------------".
idtac " ".

idtac "#> swap_if_branches".
idtac "Possible points: 3".
check_type @swap_if_branches (
(forall (b : bexp) (c1 c2 : com), cequiv (CIf b c1 c2) (CIf (BNot b) c2 c1))).
idtac "Assumptions:".
Abort.
Print Assumptions swap_if_branches.
Goal True.
idtac " ".

idtac "-------------------  CIf_congruence  --------------------".
idtac " ".

idtac "#> CIf_congruence".
idtac "Possible points: 3".
check_type @CIf_congruence (
(forall (b b' : bexp) (c1 c1' c2 c2' : com) (_ : bequiv b b')
   (_ : cequiv c1 c1') (_ : cequiv c2 c2'),
 cequiv (CIf b c1 c2) (CIf b' c1' c2'))).
idtac "Assumptions:".
Abort.
Print Assumptions CIf_congruence.
Goal True.
idtac " ".

idtac "-------------------  inequiv_exercise  --------------------".
idtac " ".

idtac "#> inequiv_exercise".
idtac "Possible points: 3".
check_type @inequiv_exercise ((not (cequiv (CWhile BTrue CSkip) CSkip))).
idtac "Assumptions:".
Abort.
Print Assumptions inequiv_exercise.
Goal True.
idtac " ".

idtac " ".

idtac "Max points - standard: 13".
idtac "Max points - advanced: 13".
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
idtac "---------- skip_right ---------".
Print Assumptions skip_right.
idtac "---------- if_false ---------".
Print Assumptions if_false.
idtac "---------- swap_if_branches ---------".
Print Assumptions swap_if_branches.
idtac "---------- CIf_congruence ---------".
Print Assumptions CIf_congruence.
idtac "---------- inequiv_exercise ---------".
Print Assumptions inequiv_exercise.
idtac "".
idtac "********** Advanced **********".
Abort.

(* 2025-10-08 12:45 *)

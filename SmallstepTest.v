Set Warnings "-notation-overridden,-parsing".
From Stdlib Require Export String.
From PLF Require Import Smallstep.

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

From PLF Require Import Smallstep.
Import Check.

Goal True.

idtac "-------------------  test_step_2  --------------------".
idtac " ".

idtac "#> SimpleArith1.test_step_2".
idtac "Possible points: 1".
check_type @SimpleArith1.test_step_2 (
(SimpleArith1.step (P (C 0) (P (C 2) (P (C 1) (C 3))))
   (P (C 0) (P (C 2) (C 4))))).
idtac "Assumptions:".
Abort.
Print Assumptions SimpleArith1.test_step_2.
Goal True.
idtac " ".

idtac "-------------------  strong_progress_bool  --------------------".
idtac " ".

idtac "#> Temp4.strong_progress_bool".
idtac "Possible points: 3".
check_type @Temp4.strong_progress_bool (
(forall t : Temp4.tm,
 or (Temp4.value t) (@ex Temp4.tm (fun t' : Temp4.tm => Temp4.step t t')))).
idtac "Assumptions:".
Abort.
Print Assumptions Temp4.strong_progress_bool.
Goal True.
idtac " ".

idtac "-------------------  multistep_congr_2  --------------------".
idtac " ".

idtac "#> multistep_congr_2".
idtac "Possible points: 2".
check_type @multistep_congr_2 (
(forall (v1 t2 t2' : tm) (_ : value v1) (_ : @multi tm step t2 t2'),
 @multi tm step (P v1 t2) (P v1 t2'))).
idtac "Assumptions:".
Abort.
Print Assumptions multistep_congr_2.
Goal True.
idtac " ".

idtac "-------------------  eval__multistep  --------------------".
idtac " ".

idtac "#> eval__multistep".
idtac "Possible points: 3".
check_type @eval__multistep (
(forall (t : tm) (n : nat) (_ : eval t n), @multi tm step t (C n))).
idtac "Assumptions:".
Abort.
Print Assumptions eval__multistep.
Goal True.
idtac " ".

idtac "-------------------  step__eval  --------------------".
idtac " ".

idtac "#> step__eval".
idtac "Possible points: 3".
check_type @step__eval (
(forall (t t' : tm) (n : nat) (_ : step t t') (_ : eval t' n), eval t n)).
idtac "Assumptions:".
Abort.
Print Assumptions step__eval.
Goal True.
idtac " ".

idtac "-------------------  multistep__eval  --------------------".
idtac " ".

idtac "#> multistep__eval".
idtac "Possible points: 3".
check_type @multistep__eval (
(forall (t t' : tm) (_ : normal_form_of t t'),
 @ex nat (fun n : nat => and (@eq tm t' (C n)) (eval t n)))).
idtac "Assumptions:".
Abort.
Print Assumptions multistep__eval.
Goal True.
idtac " ".

idtac " ".

idtac "Max points - standard: 15".
idtac "Max points - advanced: 15".
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
idtac "---------- SimpleArith1.test_step_2 ---------".
Print Assumptions SimpleArith1.test_step_2.
idtac "---------- Temp4.strong_progress_bool ---------".
Print Assumptions Temp4.strong_progress_bool.
idtac "---------- multistep_congr_2 ---------".
Print Assumptions multistep_congr_2.
idtac "---------- eval__multistep ---------".
Print Assumptions eval__multistep.
idtac "---------- step__eval ---------".
Print Assumptions step__eval.
idtac "---------- multistep__eval ---------".
Print Assumptions multistep__eval.
idtac "".
idtac "********** Advanced **********".
Abort.

(* 2025-11-06 10:50 *)

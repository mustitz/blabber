Require Import VST.floyd.proofauto.
Require Import VC.sumarray.
#[export] Instance CompSpecs : compspecs. make_compspecs prog. Defined.
Definition Vprog : varspecs.  mk_varspecs prog. Defined.

Definition sum_Z : list Z -> Z := fold_right Z.add 0.

Lemma sum_Z_one_elem : forall x, sum_Z [x] = x.
Proof.
  intro. simpl. lia.
Qed.

Lemma sum_Z_app:
  forall a b, sum_Z (a++b) =  sum_Z a + sum_Z b.
Proof.
  intros. induction a; simpl; lia.
Qed.

Definition sumarray_spec : ident * funspec :=
DECLARE _sumarray
 WITH a: val, sh : share, contents : list Z, size: Z
 PRE [ tptr tuint, tint ]
  PROP  (readable_share sh; 0 <= size <= Int.max_signed;
         Forall (fun x => 0 <= x <= Int.max_unsigned) contents)
  PARAMS (a; Vint (Int.repr size))
  SEP   (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a)
 POST [ tuint ]
  PROP () RETURN (Vint (Int.repr (sum_Z contents)))
  SEP (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a).

Definition main_spec :=
 DECLARE _main
  WITH gv : globals
  PRE  [] main_pre prog tt gv
  POST [ tint ]
     PROP()
     RETURN (Vint (Int.repr (1+2+3+4)))
     SEP(TT).

Definition Gprog := [sumarray_spec; main_spec].

Lemma body_sumarray: semax_body Vprog Gprog f_sumarray sumarray_spec.
Proof.
  start_function.
  forward. (* i = 0; *)
  forward. (* s = 0; *)
  forward_while
   (EX i: Z,
     PROP  (0 <= i <= size)
     LOCAL (temp _a a;
            temp _i (Vint (Int.repr i));
            temp _n (Vint (Int.repr size));
            temp _s (Vint (Int.repr (sum_Z (sublist 0 i contents)))))
     SEP   (data_at sh (tarray tuint size) (map Vint (map Int.repr contents)) a)).

  - (* Check invariant before *)
    Exists 0. entailer!.

  - (* i<n *)
    entailer!.

  - (* loop body *)
    assert_PROP (Zlength contents = size). {
      entailer!.
      do 2 rewrite Zlength_map. reflexivity.
    }
    forward. (* t' = a[i] *)
    forward. (* s = s + t' *)
    forward. (* i = i + 1 *)
    Exists (i+1).
    entailer!. f_equal. f_equal.
    rewrite (sublist_split 0 i (i+1)) by lia.
    rewrite sum_Z_app. rewrite (sublist_one i) by lia.
    simpl. lia.

  - (* Continue execution after loop *)
    forward.  (* return s; *)
    entailer!.
    autorewrite with sublist in *|-.
    autorewrite with sublist.
    reflexivity.
Qed.

Definition four_contents := [1; 2; 3; 4].

Lemma body_main:  semax_body Vprog Gprog f_main main_spec.
Proof.
  start_function.

  forward_call (*  s = sumarray(four,4); *)
    (gv _four, Ews, four_contents, 4).

  repeat constructor; computable.

  forward. (* return s; *)
Qed.

#[export] Existing Instance NullExtension.Espec.

Lemma prog_correct: semax_prog prog tt Vprog Gprog.
Proof.
prove_semax_prog.

semax_func_cons body_sumarray.
semax_func_cons body_main.
Qed.

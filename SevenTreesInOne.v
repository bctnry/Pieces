Module SevenTreesInOne.
Inductive tree : Set := X | Y : tree -> tree -> tree.
Notation "[ A , B ]" := (Y A B).
Definition combine
  (t : tree * tree * tree * tree * tree * tree * tree)
  : tree :=
  match t with
    | ([A,B],C,D,E,F,G,H) => [[[[[[H,G],F],E],D],C],[A,B]]
    | (A,[B,C],D,E,F,G,H) => [[[[[[H,G],F],E],D],[B,C]],A]
    | (A,B,[C,D],E,F,G,H) => [[[[[[H,G],F],E],[C,D]],B],A]
    | (A,B,C,[D,E],F,G,H) => [[[[[[H,G],F],[D,E]],C],B],A]
    | (X,X,X,X,[A,B],C,D) => [[[[X,D],C],A],B]
    | (X,X,X,X,X,[A,B],C) => [[[[[[A,B],C],X],X],X],X]
    | (X,X,X,X,X,X,[[[[A,B],C],D],E]) => [[[[[X,A],B],C],D],E]
    | (_,_,_,_,_,_,Z) => Z
  end.
Definition split
  (t : tree)
  : tree * tree * tree * tree * tree * tree * tree :=
  match t with
    | [[[[X,D],C],A],B] => (X,X,X,X,[A,B],C,D)
    | [[[[[[A,B],C],X],X],X],X] => (X,X,X,X,X,[A,B],C)
    | [[[[[X,A],B],C],D],E] => (X,X,X,X,X,X,[[[[A,B],C],D],E])
    | [[[[[[A,B],C],D],E],F],G] => (G,F,E,D,C,B,A)
    | _ => (X,X,X,X,X,X,t)
  end.
Ltac drefD x a := destruct x as [| a]; try reflexivity.
Ltac drefC x v := destruct x as [x v]; try reflexivity.
Ltac zfold x := unfold x; fold x.
Ltac sref := simpl; reflexivity.
Goal forall T, combine (split T) = T.
Proof.
    intros. do 4 drefD T T.
      destruct T. sref.
      destruct T5. sref.
      destruct T5_1. zfold split.
      destruct T4. destruct T3. destruct T2. destruct T1;
      do 2 sref. simpl. destruct T1; sref. zfold combine.
      destruct T1; destruct T2; sref. zfold combine.
      destruct T1; destruct T2; destruct T3; sref.
      zfold split. destruct T4; destruct T3; destruct T2; destruct T1; sref.
Qed.
Goal forall T, split (combine T) = T.
Proof.
    intros.
      drefC T T1. drefC T T2. drefC T T3.
      drefC T T4. drefC T T5. drefC T T6.
      destruct T; destruct T6; destruct T5;
      destruct T4; destruct T3; destruct T2; destruct T1;
      simpl; try reflexivity.
      destruct T1_1. sref. destruct T1_1_1. sref. destruct T1_1_1_1; sref.
Qed.
End SevenTreesInOne.

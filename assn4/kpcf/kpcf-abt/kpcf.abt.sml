structure KPCF :> KPCF =
struct

  structure TypOps =
  struct
    datatype typ_oper =
      Typ'Cont of typ
    | Typ'Unit
    | Typ'Prod of typ * typ
    | Typ'Void
    | Typ'Sum of typ * typ
    | Typ'Arrow of typ * typ
    | Typ'Nat
    | Typ'A
    | Typ'B
    | Typ'C
    | Typ'D
    withtype typ = typ_oper Abt.t

    fun typ_oper_bind x i typ =
      (case typ of
         (Typ'Cont typ'1) => (Typ'Cont (Abt.bind typ_oper_bind x i typ'1))
       | Typ'Unit => Typ'Unit
       | (Typ'Prod (typ'2, typ'3)) =>
           (Typ'Prod
              ( (Abt.bind typ_oper_bind x i typ'2)
              , (Abt.bind typ_oper_bind x i typ'3)
              ))
       | Typ'Void => Typ'Void
       | (Typ'Sum (typ'4, typ'5)) =>
           (Typ'Sum
              ( (Abt.bind typ_oper_bind x i typ'4)
              , (Abt.bind typ_oper_bind x i typ'5)
              ))
       | (Typ'Arrow (typ'6, typ'7)) =>
           (Typ'Arrow
              ( (Abt.bind typ_oper_bind x i typ'6)
              , (Abt.bind typ_oper_bind x i typ'7)
              ))
       | Typ'Nat => Typ'Nat
       | Typ'A => Typ'A
       | Typ'B => Typ'B
       | Typ'C => Typ'C
       | Typ'D => Typ'D)

    fun typ_oper_unbind x i typ =
      (case typ of
         (Typ'Cont typ'1) => (Typ'Cont (Abt.unbind typ_oper_unbind x i typ'1))
       | Typ'Unit => Typ'Unit
       | (Typ'Prod (typ'2, typ'3)) =>
           (Typ'Prod
              ( (Abt.unbind typ_oper_unbind x i typ'2)
              , (Abt.unbind typ_oper_unbind x i typ'3)
              ))
       | Typ'Void => Typ'Void
       | (Typ'Sum (typ'4, typ'5)) =>
           (Typ'Sum
              ( (Abt.unbind typ_oper_unbind x i typ'4)
              , (Abt.unbind typ_oper_unbind x i typ'5)
              ))
       | (Typ'Arrow (typ'6, typ'7)) =>
           (Typ'Arrow
              ( (Abt.unbind typ_oper_unbind x i typ'6)
              , (Abt.unbind typ_oper_unbind x i typ'7)
              ))
       | Typ'Nat => Typ'Nat
       | Typ'A => Typ'A
       | Typ'B => Typ'B
       | Typ'C => Typ'C
       | Typ'D => Typ'D)

    fun typ_oper_aequiv (typ1, typ2) =
      (case (typ1, typ2) of
         ((Typ'Cont typ'1), (Typ'Cont typ'2)) =>
           ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
       | (Typ'Unit, Typ'Unit) => true
       | ((Typ'Prod (typ'1, typ'3)), (Typ'Prod (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | (Typ'Void, Typ'Void) => true
       | ((Typ'Sum (typ'1, typ'3)), (Typ'Sum (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | ((Typ'Arrow (typ'1, typ'3)), (Typ'Arrow (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | (Typ'Nat, Typ'Nat) => true
       | (Typ'A, Typ'A) => true
       | (Typ'B, Typ'B) => true
       | (Typ'C, Typ'C) => true
       | (Typ'D, Typ'D) => true
       | _ => false)
  end

  structure Typ =
  struct
    type typ = TypOps.typ
    type t = typ

    datatype view =
      Cont of typ
    | Unit
    | Prod of typ * typ
    | Void
    | Sum of typ * typ
    | Arrow of typ * typ
    | Nat
    | A
    | B
    | C
    | D

    fun view_in vars typ =
      (case typ of
         (Cont typ'1) =>
           (Abt.Oper (TypOps.Typ'Cont ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'1 vars)
              , []
              ))))
       | Unit => (Abt.Oper TypOps.Typ'Unit)
       | (Prod (typ'2, typ'3)) =>
           (Abt.Oper (TypOps.Typ'Prod ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'3 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Void => (Abt.Oper TypOps.Typ'Void)
       | (Sum (typ'4, typ'5)) =>
           (Abt.Oper (TypOps.Typ'Sum ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'5 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Arrow (typ'6, typ'7)) =>
           (Abt.Oper (TypOps.Typ'Arrow ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'6 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Nat => (Abt.Oper TypOps.Typ'Nat)
       | A => (Abt.Oper TypOps.Typ'A)
       | B => (Abt.Oper TypOps.Typ'B)
       | C => (Abt.Oper TypOps.Typ'C)
       | D => (Abt.Oper TypOps.Typ'D))

    fun oper_view_out vars typ =
      (case typ of
         (TypOps.Typ'Cont typ'1) =>
           (Cont ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out TypOps.typ_oper_unbind
                             (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) typ'1 vars)
              , []
              )))
       | TypOps.Typ'Unit => Unit
       | (TypOps.Typ'Prod (typ'2, typ'3)) =>
           (Prod ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'3 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | TypOps.Typ'Void => Void
       | (TypOps.Typ'Sum (typ'4, typ'5)) =>
           (Sum ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'5 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (TypOps.Typ'Arrow (typ'6, typ'7)) =>
           (Arrow ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'6 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | TypOps.Typ'Nat => Nat
       | TypOps.Typ'A => A
       | TypOps.Typ'B => B
       | TypOps.Typ'C => C
       | TypOps.Typ'D => D)

    fun view_out t =
      (case t of
         (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into TypOps.typ_oper_bind (view_in [] v))
    fun out typ =
      (view_out (Abt.out TypOps.typ_oper_unbind typ))

    val Cont' = (into o Cont)
    val Unit' = (into Unit)
    val Prod' = (into o Prod)
    val Void' = (into Void)
    val Sum' = (into o Sum)
    val Arrow' = (into o Arrow)
    val Nat' = (into Nat)
    val A' = (into A)
    val B' = (into B)
    val C' = (into C)
    val D' = (into D)
  end

  fun typ_toString typ =
    (case (Typ.out typ) of
       (Typ.Cont typ'1) => ("(Cont " ^ (typ_toString typ'1) ^ ")")
     | Typ.Unit => "Unit"
     | (Typ.Prod (typ'2, typ'3)) =>
         ("(Prod "
          ^ ("(" ^ (typ_toString typ'2) ^ ", " ^ (typ_toString typ'3) ^ ")")
          ^ ")")
     | Typ.Void => "Void"
     | (Typ.Sum (typ'4, typ'5)) =>
         ("(Sum "
          ^ ("(" ^ (typ_toString typ'4) ^ ", " ^ (typ_toString typ'5) ^ ")")
          ^ ")")
     | (Typ.Arrow (typ'6, typ'7)) =>
         ("(Arrow "
          ^ ("(" ^ (typ_toString typ'6) ^ ", " ^ (typ_toString typ'7) ^ ")")
          ^ ")")
     | Typ.Nat => "Nat"
     | Typ.A => "A"
     | Typ.B => "B"
     | Typ.C => "C"
     | Typ.D => "D")

  structure Typ =
  struct
    open Typ
    val toString = typ_toString
    val aequiv = (Abt.aequiv TypOps.typ_oper_aequiv)
  end

  structure ExpOps =
  struct
    datatype exp_oper =
      Exp'Let of exp * (string * exp)
    | Exp'Letcc of Typ.t * (string * exp)
    | Exp'Throw of Typ.t * exp * exp
    | Exp'Unit
    | Exp'Tuple of exp * exp
    | Exp'Split of exp * ((string * string) * exp)
    | Exp'Abort of Typ.t * exp
    | Exp'InjL of (Typ.t * Typ.t) * exp
    | Exp'InjR of (Typ.t * Typ.t) * exp
    | Exp'Case of exp * (string * exp) * (string * exp)
    | Exp'Fun of (Typ.t * Typ.t) * ((string * string) * exp)
    | Exp'Lam of (string * Typ.t) * exp
    | Exp'Ap of exp * exp
    | Exp'Zero
    | Exp'Succ of exp
    | Exp'Ifz of exp * exp * (string * exp)
    withtype exp = exp_oper Abt.t

    fun exp_oper_bind x i exp =
      (case exp of
         (Exp'Let (exp'1, (exp'2, exp'3))) =>
           (Exp'Let
              ( (Abt.bind exp_oper_bind x i exp'1)
              , (exp'2, (Abt.bind exp_oper_bind x i exp'3))
              ))
       | (Exp'Letcc (typ'4, (exp'5, exp'6))) =>
           (Exp'Letcc
              ( (Abt.bind TypOps.typ_oper_bind x i typ'4)
              , (exp'5, (Abt.bind exp_oper_bind x i exp'6))
              ))
       | (Exp'Throw (typ'7, exp'8, exp'9)) =>
           (Exp'Throw
              ( (Abt.bind TypOps.typ_oper_bind x i typ'7)
              , (Abt.bind exp_oper_bind x i exp'8)
              , (Abt.bind exp_oper_bind x i exp'9)
              ))
       | Exp'Unit => Exp'Unit
       | (Exp'Tuple (exp'10, exp'11)) =>
           (Exp'Tuple
              ( (Abt.bind exp_oper_bind x i exp'10)
              , (Abt.bind exp_oper_bind x i exp'11)
              ))
       | (Exp'Split (exp'12, ((exp'13, exp'14), exp'15))) =>
           (Exp'Split
              ( (Abt.bind exp_oper_bind x i exp'12)
              , ((exp'13, exp'14), (Abt.bind exp_oper_bind x i exp'15))
              ))
       | (Exp'Abort (typ'16, exp'17)) =>
           (Exp'Abort
              ( (Abt.bind TypOps.typ_oper_bind x i typ'16)
              , (Abt.bind exp_oper_bind x i exp'17)
              ))
       | (Exp'InjL ((typ'18, typ'19), exp'20)) =>
           (Exp'InjL
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'18)
                , (Abt.bind TypOps.typ_oper_bind x i typ'19)
                )
              , (Abt.bind exp_oper_bind x i exp'20)
              ))
       | (Exp'InjR ((typ'21, typ'22), exp'23)) =>
           (Exp'InjR
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'21)
                , (Abt.bind TypOps.typ_oper_bind x i typ'22)
                )
              , (Abt.bind exp_oper_bind x i exp'23)
              ))
       | (Exp'Case (exp'24, (exp'25, exp'26), (exp'27, exp'28))) =>
           (Exp'Case
              ( (Abt.bind exp_oper_bind x i exp'24)
              , (exp'25, (Abt.bind exp_oper_bind x i exp'26))
              , (exp'27, (Abt.bind exp_oper_bind x i exp'28))
              ))
       | (Exp'Fun ((typ'29, typ'30), ((exp'31, exp'32), exp'33))) =>
           (Exp'Fun
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'29)
                , (Abt.bind TypOps.typ_oper_bind x i typ'30)
                )
              , ((exp'31, exp'32), (Abt.bind exp_oper_bind x i exp'33))
              ))
       | (Exp'Lam ((exp'34, typ'35), exp'36)) =>
           (Exp'Lam
              ( (exp'34, (Abt.bind TypOps.typ_oper_bind x i typ'35))
              , (Abt.bind exp_oper_bind x i exp'36)
              ))
       | (Exp'Ap (exp'37, exp'38)) =>
           (Exp'Ap
              ( (Abt.bind exp_oper_bind x i exp'37)
              , (Abt.bind exp_oper_bind x i exp'38)
              ))
       | Exp'Zero => Exp'Zero
       | (Exp'Succ exp'39) => (Exp'Succ (Abt.bind exp_oper_bind x i exp'39))
       | (Exp'Ifz (exp'40, exp'41, (exp'42, exp'43))) =>
           (Exp'Ifz
              ( (Abt.bind exp_oper_bind x i exp'40)
              , (Abt.bind exp_oper_bind x i exp'41)
              , (exp'42, (Abt.bind exp_oper_bind x i exp'43))
              )))

    fun exp_oper_unbind x i exp =
      (case exp of
         (Exp'Let (exp'1, (exp'2, exp'3))) =>
           (Exp'Let
              ( (Abt.unbind exp_oper_unbind x i exp'1)
              , (exp'2, (Abt.unbind exp_oper_unbind x i exp'3))
              ))
       | (Exp'Letcc (typ'4, (exp'5, exp'6))) =>
           (Exp'Letcc
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'4)
              , (exp'5, (Abt.unbind exp_oper_unbind x i exp'6))
              ))
       | (Exp'Throw (typ'7, exp'8, exp'9)) =>
           (Exp'Throw
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'7)
              , (Abt.unbind exp_oper_unbind x i exp'8)
              , (Abt.unbind exp_oper_unbind x i exp'9)
              ))
       | Exp'Unit => Exp'Unit
       | (Exp'Tuple (exp'10, exp'11)) =>
           (Exp'Tuple
              ( (Abt.unbind exp_oper_unbind x i exp'10)
              , (Abt.unbind exp_oper_unbind x i exp'11)
              ))
       | (Exp'Split (exp'12, ((exp'13, exp'14), exp'15))) =>
           (Exp'Split
              ( (Abt.unbind exp_oper_unbind x i exp'12)
              , ((exp'13, exp'14), (Abt.unbind exp_oper_unbind x i exp'15))
              ))
       | (Exp'Abort (typ'16, exp'17)) =>
           (Exp'Abort
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'16)
              , (Abt.unbind exp_oper_unbind x i exp'17)
              ))
       | (Exp'InjL ((typ'18, typ'19), exp'20)) =>
           (Exp'InjL
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'18)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'19)
                )
              , (Abt.unbind exp_oper_unbind x i exp'20)
              ))
       | (Exp'InjR ((typ'21, typ'22), exp'23)) =>
           (Exp'InjR
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'21)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'22)
                )
              , (Abt.unbind exp_oper_unbind x i exp'23)
              ))
       | (Exp'Case (exp'24, (exp'25, exp'26), (exp'27, exp'28))) =>
           (Exp'Case
              ( (Abt.unbind exp_oper_unbind x i exp'24)
              , (exp'25, (Abt.unbind exp_oper_unbind x i exp'26))
              , (exp'27, (Abt.unbind exp_oper_unbind x i exp'28))
              ))
       | (Exp'Fun ((typ'29, typ'30), ((exp'31, exp'32), exp'33))) =>
           (Exp'Fun
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'29)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'30)
                )
              , ((exp'31, exp'32), (Abt.unbind exp_oper_unbind x i exp'33))
              ))
       | (Exp'Lam ((exp'34, typ'35), exp'36)) =>
           (Exp'Lam
              ( (exp'34, (Abt.unbind TypOps.typ_oper_unbind x i typ'35))
              , (Abt.unbind exp_oper_unbind x i exp'36)
              ))
       | (Exp'Ap (exp'37, exp'38)) =>
           (Exp'Ap
              ( (Abt.unbind exp_oper_unbind x i exp'37)
              , (Abt.unbind exp_oper_unbind x i exp'38)
              ))
       | Exp'Zero => Exp'Zero
       | (Exp'Succ exp'39) => (Exp'Succ (Abt.unbind exp_oper_unbind x i exp'39))
       | (Exp'Ifz (exp'40, exp'41, (exp'42, exp'43))) =>
           (Exp'Ifz
              ( (Abt.unbind exp_oper_unbind x i exp'40)
              , (Abt.unbind exp_oper_unbind x i exp'41)
              , (exp'42, (Abt.unbind exp_oper_unbind x i exp'43))
              )))

    fun exp_oper_aequiv (exp1, exp2) =
      (case (exp1, exp2) of
         ((Exp'Let (exp'1, (_, exp'3))), (Exp'Let (exp'2, (_, exp'4)))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Letcc (typ'1, (_, exp'3))), (Exp'Letcc (typ'2, (_, exp'4)))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Throw (typ'1, exp'3, exp'5)), (Exp'Throw (typ'2, exp'4, exp'6))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
       | (Exp'Unit, Exp'Unit) => true
       | ((Exp'Tuple (exp'1, exp'3)), (Exp'Tuple (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ( (Exp'Split (exp'1, ((_, _), exp'3)))
         , (Exp'Split (exp'2, ((_, _), exp'4)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso
            ((true andalso true)
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Abort (typ'1, exp'3)), (Exp'Abort (typ'2, exp'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ( (Exp'InjL ((typ'1, typ'3), exp'5))
         , (Exp'InjL ((typ'2, typ'4), exp'6))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
       | ( (Exp'InjR ((typ'1, typ'3), exp'5))
         , (Exp'InjR ((typ'2, typ'4), exp'6))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
       | ( (Exp'Case (exp'1, (_, exp'3), (_, exp'5)))
         , (Exp'Case (exp'2, (_, exp'4), (_, exp'6)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ( (Exp'Fun ((typ'1, typ'3), ((_, _), exp'5)))
         , (Exp'Fun ((typ'2, typ'4), ((_, _), exp'6)))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso
            ((true andalso true)
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ((Exp'Lam ((_, typ'1), exp'3)), (Exp'Lam ((_, typ'2), exp'4))) =>
           ((true andalso (Typ.aequiv (typ'1, typ'2)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Ap (exp'1, exp'3)), (Exp'Ap (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | (Exp'Zero, Exp'Zero) => true
       | ((Exp'Succ exp'1), (Exp'Succ exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ( (Exp'Ifz (exp'1, exp'3, (_, exp'5)))
         , (Exp'Ifz (exp'2, exp'4, (_, exp'6)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | _ => false)
  end

  structure Exp =
  struct
    type expVar = Temp.t
    type exp = ExpOps.exp
    type t = exp

    structure Var = Temp

    datatype view =
      Var of expVar
    | Let of exp * (expVar * exp)
    | Letcc of Typ.t * (expVar * exp)
    | Throw of Typ.t * exp * exp
    | Unit
    | Tuple of exp * exp
    | Split of exp * ((expVar * expVar) * exp)
    | Abort of Typ.t * exp
    | InjL of (Typ.t * Typ.t) * exp
    | InjR of (Typ.t * Typ.t) * exp
    | Case of exp * (expVar * exp) * (expVar * exp)
    | Fun of (Typ.t * Typ.t) * ((expVar * expVar) * exp)
    | Lam of (expVar * Typ.t) * exp
    | Ap of exp * exp
    | Zero
    | Succ of exp
    | Ifz of exp * exp * (expVar * exp)

    fun view_in vars exp =
      (case exp of
         (Var x) => (Abt.Var x)
       | (Let (exp'1, (exp'2, exp'3))) =>
           (Abt.Oper (ExpOps.Exp'Let ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'1 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'2
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'3 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Letcc (typ'4, (exp'5, exp'6))) =>
           (Abt.Oper (ExpOps.Exp'Letcc ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'5
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'6 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Throw (typ'7, exp'8, exp'9)) =>
           (Abt.Oper (ExpOps.Exp'Throw ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'7 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'8 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'9 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | Unit => (Abt.Oper ExpOps.Exp'Unit)
       | (Tuple (exp'10, exp'11)) =>
           (Abt.Oper (ExpOps.Exp'Tuple ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'10 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'11 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Split (exp'12, ((exp'13, exp'14), exp'15))) =>
           (Abt.Oper (ExpOps.Exp'Split ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'12 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val var = exp'13
                                           in ((Temp.toUserString var), [var])
                                           end
                        val (t1, vars'1) = let val var = exp'14
                                           in ((Temp.toUserString var), [var])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'15 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Abort (typ'16, exp'17)) =>
           (Abt.Oper (ExpOps.Exp'Abort ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'16 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'17 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (InjL ((typ'18, typ'19), exp'20)) =>
           (Abt.Oper (ExpOps.Exp'InjL ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'18 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'19 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'20 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (InjR ((typ'21, typ'22), exp'23)) =>
           (Abt.Oper (ExpOps.Exp'InjR ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'21 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'22 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'23 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Case (exp'24, (exp'25, exp'26), (exp'27, exp'28))) =>
           (Abt.Oper (ExpOps.Exp'Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'24 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'25
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'26 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = exp'27
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'28 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Fun ((typ'29, typ'30), ((exp'31, exp'32), exp'33))) =>
           (Abt.Oper (ExpOps.Exp'Fun ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'29 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'30 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val var = exp'31
                                           in ((Temp.toUserString var), [var])
                                           end
                        val (t1, vars'1) = let val var = exp'32
                                           in ((Temp.toUserString var), [var])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'33 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Lam ((exp'34, typ'35), exp'36)) =>
           (Abt.Oper (ExpOps.Exp'Lam ((fn (x, _) => x)
              let
                val (t, vars') =
                  let
                    val (t0, vars'0) = let val var = exp'34
                                       in ((Temp.toUserString var), [var])
                                       end
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'35 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val vars = (vars' @ vars)
                val (t', vars') =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'36 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end)))
       | (Ap (exp'37, exp'38)) =>
           (Abt.Oper (ExpOps.Exp'Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'37 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'38 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Zero => (Abt.Oper ExpOps.Exp'Zero)
       | (Succ exp'39) =>
           (Abt.Oper (ExpOps.Exp'Succ ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'39 vars)
              , []
              ))))
       | (Ifz (exp'40, exp'41, (exp'42, exp'43))) =>
           (Abt.Oper (ExpOps.Exp'Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'40 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'41 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = exp'42
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'43 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))))

    fun oper_view_out vars exp =
      (case exp of
         (ExpOps.Exp'Let (exp'1, (exp'2, exp'3))) =>
           (Let ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'1 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'2)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'3 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Letcc (typ'4, (exp'5, exp'6))) =>
           (Letcc ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'5)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'6 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Throw (typ'7, exp'8, exp'9)) =>
           (Throw ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'7 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'8 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'9 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | ExpOps.Exp'Unit => Unit
       | (ExpOps.Exp'Tuple (exp'10, exp'11)) =>
           (Tuple ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'10 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'11 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Split (exp'12, ((exp'13, exp'14), exp'15))) =>
           (Split ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'12 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val x = (Temp.new exp'13)
                                           in (x, [x])
                                           end
                        val (t1, vars'1) = let val x = (Temp.new exp'14)
                                           in (x, [x])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'15 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Abort (typ'16, exp'17)) =>
           (Abort ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out TypOps.typ_oper_unbind
                                 (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) typ'16 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'17 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'InjL ((typ'18, typ'19), exp'20)) =>
           (InjL ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'18 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'19 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'20 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'InjR ((typ'21, typ'22), exp'23)) =>
           (InjR ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'21 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'22 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'23 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Case (exp'24, (exp'25, exp'26), (exp'27, exp'28))) =>
           (Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'24 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'25)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'26 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'27)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'28 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ExpOps.Exp'Fun ((typ'29, typ'30), ((exp'31, exp'32), exp'33))) =>
           (Fun ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'29 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'30 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val x = (Temp.new exp'31)
                                           in (x, [x])
                                           end
                        val (t1, vars'1) = let val x = (Temp.new exp'32)
                                           in (x, [x])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'33 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Lam ((exp'34, typ'35), exp'36)) =>
           (Lam ((fn (x, _) => x)
              let
                val (t, vars') =
                  let
                    val (t0, vars'0) = let val x = (Temp.new exp'34)
                                       in (x, [x])
                                       end
                    val (t1, vars'1) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out TypOps.typ_oper_unbind
                                     (Abt.unbind TypOps.typ_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) typ'35 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val vars = (vars' @ vars)
                val (t', vars') =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'36 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end))
       | (ExpOps.Exp'Ap (exp'37, exp'38)) =>
           (Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'37 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'38 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | ExpOps.Exp'Zero => Zero
       | (ExpOps.Exp'Succ exp'39) =>
           (Succ ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpOps.exp_oper_unbind
                             (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'39 vars)
              , []
              )))
       | (ExpOps.Exp'Ifz (exp'40, exp'41, (exp'42, exp'43))) =>
           (Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'40 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'41 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'42)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpOps.exp_oper_unbind
                                     (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                              in
                                acc'
                              end) exp'43 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))

    fun view_out t =
      (case t of
         (Abt.Var x) => (Var x)
       | (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ExpOps.exp_oper_bind (view_in [] v))
    fun out exp =
      (view_out (Abt.out ExpOps.exp_oper_unbind exp))

    val Var' = (into o Var)
    val Let' = (into o Let)
    val Letcc' = (into o Letcc)
    val Throw' = (into o Throw)
    val Unit' = (into Unit)
    val Tuple' = (into o Tuple)
    val Split' = (into o Split)
    val Abort' = (into o Abort)
    val InjL' = (into o InjL)
    val InjR' = (into o InjR)
    val Case' = (into o Case)
    val Fun' = (into o Fun)
    val Lam' = (into o Lam)
    val Ap' = (into o Ap)
    val Zero' = (into Zero)
    val Succ' = (into o Succ)
    val Ifz' = (into o Ifz)
  end

  fun exp_toString exp =
    (case (Exp.out exp) of
       (Exp.Var x) => (Exp.Var.toString x)
     | (Exp.Let (exp'1, (exp'2, exp'3))) =>
         ("(Let "
          ^
          ("(" ^ (exp_toString exp'1) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'2) ^ " . " ^ (exp_toString exp'3) ^ ")")
           ^ ")") ^ ")")
     | (Exp.Letcc (typ'4, (exp'5, exp'6))) =>
         ("(Letcc "
          ^
          ("(" ^ (Typ.toString typ'4) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'5) ^ " . " ^ (exp_toString exp'6) ^ ")")
           ^ ")") ^ ")")
     | (Exp.Throw (typ'7, exp'8, exp'9)) =>
         ("(Throw "
          ^
          ("(" ^ (Typ.toString typ'7) ^ ", " ^ (exp_toString exp'8) ^ ", "
           ^ (exp_toString exp'9) ^ ")") ^ ")")
     | Exp.Unit => "Unit"
     | (Exp.Tuple (exp'10, exp'11)) =>
         ("(Tuple "
          ^ ("(" ^ (exp_toString exp'10) ^ ", " ^ (exp_toString exp'11) ^ ")")
          ^ ")")
     | (Exp.Split (exp'12, ((exp'13, exp'14), exp'15))) =>
         ("(Split "
          ^
          ("(" ^ (exp_toString exp'12) ^ ", "
           ^
           ("("
            ^
            ("(" ^ (Exp.Var.toString exp'13) ^ ", " ^ (Exp.Var.toString exp'14)
             ^ ")") ^ " . " ^ (exp_toString exp'15) ^ ")") ^ ")") ^ ")")
     | (Exp.Abort (typ'16, exp'17)) =>
         ("(Abort "
          ^ ("(" ^ (Typ.toString typ'16) ^ ", " ^ (exp_toString exp'17) ^ ")")
          ^ ")")
     | (Exp.InjL ((typ'18, typ'19), exp'20)) =>
         ("(InjL "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'18) ^ ", " ^ (Typ.toString typ'19) ^ ")")
           ^ ", " ^ (exp_toString exp'20) ^ ")") ^ ")")
     | (Exp.InjR ((typ'21, typ'22), exp'23)) =>
         ("(InjR "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'21) ^ ", " ^ (Typ.toString typ'22) ^ ")")
           ^ ", " ^ (exp_toString exp'23) ^ ")") ^ ")")
     | (Exp.Case (exp'24, (exp'25, exp'26), (exp'27, exp'28))) =>
         ("(Case "
          ^
          ("(" ^ (exp_toString exp'24) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'25) ^ " . " ^ (exp_toString exp'26)
            ^ ")") ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'27) ^ " . " ^ (exp_toString exp'28)
            ^ ")") ^ ")") ^ ")")
     | (Exp.Fun ((typ'29, typ'30), ((exp'31, exp'32), exp'33))) =>
         ("(Fun "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'29) ^ ", " ^ (Typ.toString typ'30) ^ ")")
           ^ ", "
           ^
           ("("
            ^
            ("(" ^ (Exp.Var.toString exp'31) ^ ", " ^ (Exp.Var.toString exp'32)
             ^ ")") ^ " . " ^ (exp_toString exp'33) ^ ")") ^ ")") ^ ")")
     | (Exp.Lam ((exp'34, typ'35), exp'36)) =>
         ("(Lam "
          ^
          ("("
           ^
           ("(" ^ (Exp.Var.toString exp'34) ^ ", " ^ (Typ.toString typ'35) ^ ")")
           ^ " . " ^ (exp_toString exp'36) ^ ")") ^ ")")
     | (Exp.Ap (exp'37, exp'38)) =>
         ("(Ap "
          ^ ("(" ^ (exp_toString exp'37) ^ ", " ^ (exp_toString exp'38) ^ ")")
          ^ ")")
     | Exp.Zero => "Zero"
     | (Exp.Succ exp'39) => ("(Succ " ^ (exp_toString exp'39) ^ ")")
     | (Exp.Ifz (exp'40, exp'41, (exp'42, exp'43))) =>
         ("(Ifz "
          ^
          ("(" ^ (exp_toString exp'40) ^ ", " ^ (exp_toString exp'41) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'42) ^ " . " ^ (exp_toString exp'43)
            ^ ")") ^ ")") ^ ")"))

  fun exp_subst t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (if (Exp.Var.equal (x, y)) then t else (Exp.Var' y))
     | (Exp.Let (exp'1, (exp'2, exp'3))) =>
         (Exp.Let' ((exp_subst t x exp'1), (exp'2, (exp_subst t x exp'3))))
     | (Exp.Letcc (typ'4, (exp'5, exp'6))) =>
         (Exp.Letcc' (typ'4, (exp'5, (exp_subst t x exp'6))))
     | (Exp.Throw (typ'7, exp'8, exp'9)) =>
         (Exp.Throw' (typ'7, (exp_subst t x exp'8), (exp_subst t x exp'9)))
     | Exp.Unit => Exp.Unit'
     | (Exp.Tuple (exp'10, exp'11)) =>
         (Exp.Tuple' ((exp_subst t x exp'10), (exp_subst t x exp'11)))
     | (Exp.Split (exp'12, ((exp'13, exp'14), exp'15))) =>
         (Exp.Split'
            ((exp_subst t x exp'12), ((exp'13, exp'14), (exp_subst t x exp'15))))
     | (Exp.Abort (typ'16, exp'17)) =>
         (Exp.Abort' (typ'16, (exp_subst t x exp'17)))
     | (Exp.InjL ((typ'18, typ'19), exp'20)) =>
         (Exp.InjL' ((typ'18, typ'19), (exp_subst t x exp'20)))
     | (Exp.InjR ((typ'21, typ'22), exp'23)) =>
         (Exp.InjR' ((typ'21, typ'22), (exp_subst t x exp'23)))
     | (Exp.Case (exp'24, (exp'25, exp'26), (exp'27, exp'28))) =>
         (Exp.Case'
            ( (exp_subst t x exp'24)
            , (exp'25, (exp_subst t x exp'26))
            , (exp'27, (exp_subst t x exp'28))
            ))
     | (Exp.Fun ((typ'29, typ'30), ((exp'31, exp'32), exp'33))) =>
         (Exp.Fun'
            ((typ'29, typ'30), ((exp'31, exp'32), (exp_subst t x exp'33))))
     | (Exp.Lam ((exp'34, typ'35), exp'36)) =>
         (Exp.Lam' ((exp'34, typ'35), (exp_subst t x exp'36)))
     | (Exp.Ap (exp'37, exp'38)) =>
         (Exp.Ap' ((exp_subst t x exp'37), (exp_subst t x exp'38)))
     | Exp.Zero => Exp.Zero'
     | (Exp.Succ exp'39) => (Exp.Succ' (exp_subst t x exp'39))
     | (Exp.Ifz (exp'40, exp'41, (exp'42, exp'43))) =>
         (Exp.Ifz'
            ( (exp_subst t x exp'40)
            , (exp_subst t x exp'41)
            , (exp'42, (exp_subst t x exp'43))
            )))

  structure Exp =
  struct
    open Exp
    val toString = exp_toString
    val aequiv = (Abt.aequiv ExpOps.exp_oper_aequiv)
    val subst = exp_subst
  end
end

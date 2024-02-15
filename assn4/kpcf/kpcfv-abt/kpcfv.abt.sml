structure KPCFv :> KPCFV =
struct

  structure TypOps =
  struct
    datatype typ_oper =
      Typ'Comp of typ
    | Typ'Cont of typ
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
         (Typ'Comp typ'1) => (Typ'Comp (Abt.bind typ_oper_bind x i typ'1))
       | (Typ'Cont typ'2) => (Typ'Cont (Abt.bind typ_oper_bind x i typ'2))
       | Typ'Unit => Typ'Unit
       | (Typ'Prod (typ'3, typ'4)) =>
           (Typ'Prod
              ( (Abt.bind typ_oper_bind x i typ'3)
              , (Abt.bind typ_oper_bind x i typ'4)
              ))
       | Typ'Void => Typ'Void
       | (Typ'Sum (typ'5, typ'6)) =>
           (Typ'Sum
              ( (Abt.bind typ_oper_bind x i typ'5)
              , (Abt.bind typ_oper_bind x i typ'6)
              ))
       | (Typ'Arrow (typ'7, typ'8)) =>
           (Typ'Arrow
              ( (Abt.bind typ_oper_bind x i typ'7)
              , (Abt.bind typ_oper_bind x i typ'8)
              ))
       | Typ'Nat => Typ'Nat
       | Typ'A => Typ'A
       | Typ'B => Typ'B
       | Typ'C => Typ'C
       | Typ'D => Typ'D)

    fun typ_oper_unbind x i typ =
      (case typ of
         (Typ'Comp typ'1) => (Typ'Comp (Abt.unbind typ_oper_unbind x i typ'1))
       | (Typ'Cont typ'2) => (Typ'Cont (Abt.unbind typ_oper_unbind x i typ'2))
       | Typ'Unit => Typ'Unit
       | (Typ'Prod (typ'3, typ'4)) =>
           (Typ'Prod
              ( (Abt.unbind typ_oper_unbind x i typ'3)
              , (Abt.unbind typ_oper_unbind x i typ'4)
              ))
       | Typ'Void => Typ'Void
       | (Typ'Sum (typ'5, typ'6)) =>
           (Typ'Sum
              ( (Abt.unbind typ_oper_unbind x i typ'5)
              , (Abt.unbind typ_oper_unbind x i typ'6)
              ))
       | (Typ'Arrow (typ'7, typ'8)) =>
           (Typ'Arrow
              ( (Abt.unbind typ_oper_unbind x i typ'7)
              , (Abt.unbind typ_oper_unbind x i typ'8)
              ))
       | Typ'Nat => Typ'Nat
       | Typ'A => Typ'A
       | Typ'B => Typ'B
       | Typ'C => Typ'C
       | Typ'D => Typ'D)

    fun typ_oper_aequiv (typ1, typ2) =
      (case (typ1, typ2) of
         ((Typ'Comp typ'1), (Typ'Comp typ'2)) =>
           ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
       | ((Typ'Cont typ'1), (Typ'Cont typ'2)) =>
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
      Comp of typ
    | Cont of typ
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
         (Comp typ'1) =>
           (Abt.Oper (TypOps.Typ'Comp ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'1 vars)
              , []
              ))))
       | (Cont typ'2) =>
           (Abt.Oper (TypOps.Typ'Cont ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'2 vars)
              , []
              ))))
       | Unit => (Abt.Oper TypOps.Typ'Unit)
       | (Prod (typ'3, typ'4)) =>
           (Abt.Oper (TypOps.Typ'Prod ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'3 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'4 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Void => (Abt.Oper TypOps.Typ'Void)
       | (Sum (typ'5, typ'6)) =>
           (Abt.Oper (TypOps.Typ'Sum ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'5 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'6 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Arrow (typ'7, typ'8)) =>
           (Abt.Oper (TypOps.Typ'Arrow ((fn (x, _) => x)
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
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'8 vars)
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
         (TypOps.Typ'Comp typ'1) =>
           (Comp ((fn (x, _) => x)
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
       | (TypOps.Typ'Cont typ'2) =>
           (Cont ((fn (x, _) => x)
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
              )))
       | TypOps.Typ'Unit => Unit
       | (TypOps.Typ'Prod (typ'3, typ'4)) =>
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
                          end) typ'3 vars)
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
                          end) typ'4 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | TypOps.Typ'Void => Void
       | (TypOps.Typ'Sum (typ'5, typ'6)) =>
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
                          end) typ'5 vars)
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
                          end) typ'6 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (TypOps.Typ'Arrow (typ'7, typ'8)) =>
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
                          end) typ'7 vars)
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
                          end) typ'8 vars)
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

    val Comp' = (into o Comp)
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
       (Typ.Comp typ'1) => ("(Comp " ^ (typ_toString typ'1) ^ ")")
     | (Typ.Cont typ'2) => ("(Cont " ^ (typ_toString typ'2) ^ ")")
     | Typ.Unit => "Unit"
     | (Typ.Prod (typ'3, typ'4)) =>
         ("(Prod "
          ^ ("(" ^ (typ_toString typ'3) ^ ", " ^ (typ_toString typ'4) ^ ")")
          ^ ")")
     | Typ.Void => "Void"
     | (Typ.Sum (typ'5, typ'6)) =>
         ("(Sum "
          ^ ("(" ^ (typ_toString typ'5) ^ ", " ^ (typ_toString typ'6) ^ ")")
          ^ ")")
     | (Typ.Arrow (typ'7, typ'8)) =>
         ("(Arrow "
          ^ ("(" ^ (typ_toString typ'7) ^ ", " ^ (typ_toString typ'8) ^ ")")
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

  structure ValueStackExpOps =
  struct
    datatype value_oper =
      Value'Comp of exp
    | Value'Cont of Typ.t * stack
    | Value'Unit
    | Value'Tuple of value * value
    | Value'InjL of (Typ.t * Typ.t) * value
    | Value'InjR of (Typ.t * Typ.t) * value
    | Value'Fun of (Typ.t * Typ.t) * ((string * string) * exp)
    | Value'Lam of (string * Typ.t) * exp
    | Value'Zero
    | Value'Succ of value
    and stack_oper =
      Stack'Epsilon
    | Stack'Frame of stack * (string * exp)
    and exp_oper =
      Exp'Ret of value
    | Exp'Bind of value * (string * exp)
    | Exp'Letcc of Typ.t * (string * exp)
    | Exp'Throw of Typ.t * value * value
    | Exp'Split of value * ((string * string) * exp)
    | Exp'Abort of Typ.t * value
    | Exp'Case of value * (string * exp) * (string * exp)
    | Exp'Ap of value * value
    | Exp'Ifz of value * exp * (string * exp)
    withtype value = value_oper Abt.t
    and stack = stack_oper Abt.t
    and exp = exp_oper Abt.t

    fun value_oper_bind x i value =
      (case value of
         (Value'Comp exp'1) => (Value'Comp (Abt.bind exp_oper_bind x i exp'1))
       | (Value'Cont (typ'2, stack'3)) =>
           (Value'Cont
              ( (Abt.bind TypOps.typ_oper_bind x i typ'2)
              , (Abt.bind stack_oper_bind x i stack'3)
              ))
       | Value'Unit => Value'Unit
       | (Value'Tuple (value'4, value'5)) =>
           (Value'Tuple
              ( (Abt.bind value_oper_bind x i value'4)
              , (Abt.bind value_oper_bind x i value'5)
              ))
       | (Value'InjL ((typ'6, typ'7), value'8)) =>
           (Value'InjL
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'6)
                , (Abt.bind TypOps.typ_oper_bind x i typ'7)
                )
              , (Abt.bind value_oper_bind x i value'8)
              ))
       | (Value'InjR ((typ'9, typ'10), value'11)) =>
           (Value'InjR
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'9)
                , (Abt.bind TypOps.typ_oper_bind x i typ'10)
                )
              , (Abt.bind value_oper_bind x i value'11)
              ))
       | (Value'Fun ((typ'12, typ'13), ((value'14, value'15), exp'16))) =>
           (Value'Fun
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'12)
                , (Abt.bind TypOps.typ_oper_bind x i typ'13)
                )
              , ((value'14, value'15), (Abt.bind exp_oper_bind x i exp'16))
              ))
       | (Value'Lam ((value'17, typ'18), exp'19)) =>
           (Value'Lam
              ( (value'17, (Abt.bind TypOps.typ_oper_bind x i typ'18))
              , (Abt.bind exp_oper_bind x i exp'19)
              ))
       | Value'Zero => Value'Zero
       | (Value'Succ value'20) =>
           (Value'Succ (Abt.bind value_oper_bind x i value'20)))
    and stack_oper_bind x i stack =
      (case stack of
         Stack'Epsilon => Stack'Epsilon
       | (Stack'Frame (stack'1, (value'2, exp'3))) =>
           (Stack'Frame
              ( (Abt.bind stack_oper_bind x i stack'1)
              , (value'2, (Abt.bind exp_oper_bind x i exp'3))
              )))
    and exp_oper_bind x i exp =
      (case exp of
         (Exp'Ret value'1) => (Exp'Ret (Abt.bind value_oper_bind x i value'1))
       | (Exp'Bind (value'2, (value'3, exp'4))) =>
           (Exp'Bind
              ( (Abt.bind value_oper_bind x i value'2)
              , (value'3, (Abt.bind exp_oper_bind x i exp'4))
              ))
       | (Exp'Letcc (typ'5, (value'6, exp'7))) =>
           (Exp'Letcc
              ( (Abt.bind TypOps.typ_oper_bind x i typ'5)
              , (value'6, (Abt.bind exp_oper_bind x i exp'7))
              ))
       | (Exp'Throw (typ'8, value'9, value'10)) =>
           (Exp'Throw
              ( (Abt.bind TypOps.typ_oper_bind x i typ'8)
              , (Abt.bind value_oper_bind x i value'9)
              , (Abt.bind value_oper_bind x i value'10)
              ))
       | (Exp'Split (value'11, ((value'12, value'13), exp'14))) =>
           (Exp'Split
              ( (Abt.bind value_oper_bind x i value'11)
              , ((value'12, value'13), (Abt.bind exp_oper_bind x i exp'14))
              ))
       | (Exp'Abort (typ'15, value'16)) =>
           (Exp'Abort
              ( (Abt.bind TypOps.typ_oper_bind x i typ'15)
              , (Abt.bind value_oper_bind x i value'16)
              ))
       | (Exp'Case (value'17, (value'18, exp'19), (value'20, exp'21))) =>
           (Exp'Case
              ( (Abt.bind value_oper_bind x i value'17)
              , (value'18, (Abt.bind exp_oper_bind x i exp'19))
              , (value'20, (Abt.bind exp_oper_bind x i exp'21))
              ))
       | (Exp'Ap (value'22, value'23)) =>
           (Exp'Ap
              ( (Abt.bind value_oper_bind x i value'22)
              , (Abt.bind value_oper_bind x i value'23)
              ))
       | (Exp'Ifz (value'24, exp'25, (value'26, exp'27))) =>
           (Exp'Ifz
              ( (Abt.bind value_oper_bind x i value'24)
              , (Abt.bind exp_oper_bind x i exp'25)
              , (value'26, (Abt.bind exp_oper_bind x i exp'27))
              )))

    fun value_oper_unbind x i value =
      (case value of
         (Value'Comp exp'1) =>
           (Value'Comp (Abt.unbind exp_oper_unbind x i exp'1))
       | (Value'Cont (typ'2, stack'3)) =>
           (Value'Cont
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'2)
              , (Abt.unbind stack_oper_unbind x i stack'3)
              ))
       | Value'Unit => Value'Unit
       | (Value'Tuple (value'4, value'5)) =>
           (Value'Tuple
              ( (Abt.unbind value_oper_unbind x i value'4)
              , (Abt.unbind value_oper_unbind x i value'5)
              ))
       | (Value'InjL ((typ'6, typ'7), value'8)) =>
           (Value'InjL
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'6)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'7)
                )
              , (Abt.unbind value_oper_unbind x i value'8)
              ))
       | (Value'InjR ((typ'9, typ'10), value'11)) =>
           (Value'InjR
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'9)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'10)
                )
              , (Abt.unbind value_oper_unbind x i value'11)
              ))
       | (Value'Fun ((typ'12, typ'13), ((value'14, value'15), exp'16))) =>
           (Value'Fun
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'12)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'13)
                )
              , ((value'14, value'15), (Abt.unbind exp_oper_unbind x i exp'16))
              ))
       | (Value'Lam ((value'17, typ'18), exp'19)) =>
           (Value'Lam
              ( (value'17, (Abt.unbind TypOps.typ_oper_unbind x i typ'18))
              , (Abt.unbind exp_oper_unbind x i exp'19)
              ))
       | Value'Zero => Value'Zero
       | (Value'Succ value'20) =>
           (Value'Succ (Abt.unbind value_oper_unbind x i value'20)))
    and stack_oper_unbind x i stack =
      (case stack of
         Stack'Epsilon => Stack'Epsilon
       | (Stack'Frame (stack'1, (value'2, exp'3))) =>
           (Stack'Frame
              ( (Abt.unbind stack_oper_unbind x i stack'1)
              , (value'2, (Abt.unbind exp_oper_unbind x i exp'3))
              )))
    and exp_oper_unbind x i exp =
      (case exp of
         (Exp'Ret value'1) =>
           (Exp'Ret (Abt.unbind value_oper_unbind x i value'1))
       | (Exp'Bind (value'2, (value'3, exp'4))) =>
           (Exp'Bind
              ( (Abt.unbind value_oper_unbind x i value'2)
              , (value'3, (Abt.unbind exp_oper_unbind x i exp'4))
              ))
       | (Exp'Letcc (typ'5, (value'6, exp'7))) =>
           (Exp'Letcc
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'5)
              , (value'6, (Abt.unbind exp_oper_unbind x i exp'7))
              ))
       | (Exp'Throw (typ'8, value'9, value'10)) =>
           (Exp'Throw
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'8)
              , (Abt.unbind value_oper_unbind x i value'9)
              , (Abt.unbind value_oper_unbind x i value'10)
              ))
       | (Exp'Split (value'11, ((value'12, value'13), exp'14))) =>
           (Exp'Split
              ( (Abt.unbind value_oper_unbind x i value'11)
              , ((value'12, value'13), (Abt.unbind exp_oper_unbind x i exp'14))
              ))
       | (Exp'Abort (typ'15, value'16)) =>
           (Exp'Abort
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'15)
              , (Abt.unbind value_oper_unbind x i value'16)
              ))
       | (Exp'Case (value'17, (value'18, exp'19), (value'20, exp'21))) =>
           (Exp'Case
              ( (Abt.unbind value_oper_unbind x i value'17)
              , (value'18, (Abt.unbind exp_oper_unbind x i exp'19))
              , (value'20, (Abt.unbind exp_oper_unbind x i exp'21))
              ))
       | (Exp'Ap (value'22, value'23)) =>
           (Exp'Ap
              ( (Abt.unbind value_oper_unbind x i value'22)
              , (Abt.unbind value_oper_unbind x i value'23)
              ))
       | (Exp'Ifz (value'24, exp'25, (value'26, exp'27))) =>
           (Exp'Ifz
              ( (Abt.unbind value_oper_unbind x i value'24)
              , (Abt.unbind exp_oper_unbind x i exp'25)
              , (value'26, (Abt.unbind exp_oper_unbind x i exp'27))
              )))

    fun value_oper_aequiv (value1, value2) =
      (case (value1, value2) of
         ((Value'Comp exp'1), (Value'Comp exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Value'Cont (typ'1, stack'3)), (Value'Cont (typ'2, stack'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv stack_oper_aequiv) (stack'3, stack'4)))
       | (Value'Unit, Value'Unit) => true
       | ((Value'Tuple (value'1, value'3)), (Value'Tuple (value'2, value'4))) =>
           (((Abt.aequiv value_oper_aequiv) (value'1, value'2))
            andalso ((Abt.aequiv value_oper_aequiv) (value'3, value'4)))
       | ( (Value'InjL ((typ'1, typ'3), value'5))
         , (Value'InjL ((typ'2, typ'4), value'6))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso ((Abt.aequiv value_oper_aequiv) (value'5, value'6)))
       | ( (Value'InjR ((typ'1, typ'3), value'5))
         , (Value'InjR ((typ'2, typ'4), value'6))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso ((Abt.aequiv value_oper_aequiv) (value'5, value'6)))
       | ( (Value'Fun ((typ'1, typ'3), ((_, _), exp'5)))
         , (Value'Fun ((typ'2, typ'4), ((_, _), exp'6)))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso
            ((true andalso true)
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ((Value'Lam ((_, typ'1), exp'3)), (Value'Lam ((_, typ'2), exp'4))) =>
           ((true andalso (Typ.aequiv (typ'1, typ'2)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | (Value'Zero, Value'Zero) => true
       | ((Value'Succ value'1), (Value'Succ value'2)) =>
           ((Abt.aequiv value_oper_aequiv) (value'1, value'2))
       | _ => false)
    and stack_oper_aequiv (stack1, stack2) =
      (case (stack1, stack2) of
         (Stack'Epsilon, Stack'Epsilon) => true
       | ( (Stack'Frame (stack'1, (_, exp'3)))
         , (Stack'Frame (stack'2, (_, exp'4)))
         ) =>
           (((Abt.aequiv stack_oper_aequiv) (stack'1, stack'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | _ => false)
    and exp_oper_aequiv (exp1, exp2) =
      (case (exp1, exp2) of
         ((Exp'Ret value'1), (Exp'Ret value'2)) =>
           ((Abt.aequiv value_oper_aequiv) (value'1, value'2))
       | ((Exp'Bind (value'1, (_, exp'3))), (Exp'Bind (value'2, (_, exp'4)))) =>
           (((Abt.aequiv value_oper_aequiv) (value'1, value'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Letcc (typ'1, (_, exp'3))), (Exp'Letcc (typ'2, (_, exp'4)))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ( (Exp'Throw (typ'1, value'3, value'5))
         , (Exp'Throw (typ'2, value'4, value'6))
         ) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv value_oper_aequiv) (value'3, value'4))
            andalso ((Abt.aequiv value_oper_aequiv) (value'5, value'6)))
       | ( (Exp'Split (value'1, ((_, _), exp'3)))
         , (Exp'Split (value'2, ((_, _), exp'4)))
         ) =>
           (((Abt.aequiv value_oper_aequiv) (value'1, value'2))
            andalso
            ((true andalso true)
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Abort (typ'1, value'3)), (Exp'Abort (typ'2, value'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv value_oper_aequiv) (value'3, value'4)))
       | ( (Exp'Case (value'1, (_, exp'3), (_, exp'5)))
         , (Exp'Case (value'2, (_, exp'4), (_, exp'6)))
         ) =>
           (((Abt.aequiv value_oper_aequiv) (value'1, value'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ((Exp'Ap (value'1, value'3)), (Exp'Ap (value'2, value'4))) =>
           (((Abt.aequiv value_oper_aequiv) (value'1, value'2))
            andalso ((Abt.aequiv value_oper_aequiv) (value'3, value'4)))
       | ( (Exp'Ifz (value'1, exp'3, (_, exp'5)))
         , (Exp'Ifz (value'2, exp'4, (_, exp'6)))
         ) =>
           (((Abt.aequiv value_oper_aequiv) (value'1, value'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | _ => false)
  end

  structure Value =
  struct
    type stack = ValueStackExpOps.stack
    type exp = ValueStackExpOps.exp
    type valueVar = Temp.t
    type value = ValueStackExpOps.value
    type t = value

    structure Var = Temp

    datatype view =
      Var of valueVar
    | Comp of exp
    | Cont of Typ.t * stack
    | Unit
    | Tuple of value * value
    | InjL of (Typ.t * Typ.t) * value
    | InjR of (Typ.t * Typ.t) * value
    | Fun of (Typ.t * Typ.t) * ((valueVar * valueVar) * exp)
    | Lam of (valueVar * Typ.t) * exp
    | Zero
    | Succ of value

    fun view_in vars value =
      (case value of
         (Var x) => (Abt.Var x)
       | (Comp exp'1) =>
           (Abt.Oper (ValueStackExpOps.Value'Comp ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ValueStackExpOps.exp_oper_bind
                         (Abt.Binding (x, acc)))) exp'1 vars)
              , []
              ))))
       | (Cont (typ'2, stack'3)) =>
           (Abt.Oper (ValueStackExpOps.Value'Cont ((fn (x, _) => x)
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
                          (Abt.into ValueStackExpOps.stack_oper_bind
                             (Abt.Binding (x, acc)))) stack'3 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Unit => (Abt.Oper ValueStackExpOps.Value'Unit)
       | (Tuple (value'4, value'5)) =>
           (Abt.Oper (ValueStackExpOps.Value'Tuple ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'5 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (InjL ((typ'6, typ'7), value'8)) =>
           (Abt.Oper (ValueStackExpOps.Value'InjL ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'6 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'7 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'8 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (InjR ((typ'9, typ'10), value'11)) =>
           (Abt.Oper (ValueStackExpOps.Value'InjR ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'9 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'10 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'11 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Fun ((typ'12, typ'13), ((value'14, value'15), exp'16))) =>
           (Abt.Oper (ValueStackExpOps.Value'Fun ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'12 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'13 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val var = value'14
                                           in ((Temp.toUserString var), [var])
                                           end
                        val (t1, vars'1) = let val var = value'15
                                           in ((Temp.toUserString var), [var])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'16 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Lam ((value'17, typ'18), exp'19)) =>
           (Abt.Oper (ValueStackExpOps.Value'Lam ((fn (x, _) => x)
              let
                val (t, vars') =
                  let
                    val (t0, vars'0) = let val var = value'17
                                       in ((Temp.toUserString var), [var])
                                       end
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'18 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val vars = (vars' @ vars)
                val (t', vars') =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'19 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end)))
       | Zero => (Abt.Oper ValueStackExpOps.Value'Zero)
       | (Succ value'20) =>
           (Abt.Oper (ValueStackExpOps.Value'Succ ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ValueStackExpOps.value_oper_bind
                         (Abt.Binding (x, acc)))) value'20 vars)
              , []
              )))))

    fun oper_view_out vars value =
      (case value of
         (ValueStackExpOps.Value'Comp exp'1) =>
           (Comp ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ValueStackExpOps.exp_oper_unbind
                             (Abt.unbind ValueStackExpOps.exp_oper_unbind x ~1
                                acc))
                      in
                        acc'
                      end) exp'1 vars)
              , []
              )))
       | (ValueStackExpOps.Value'Cont (typ'2, stack'3)) =>
           (Cont ((fn (x, _) => x)
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
                              (Abt.out ValueStackExpOps.stack_oper_unbind
                                 (Abt.unbind ValueStackExpOps.stack_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) stack'3 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | ValueStackExpOps.Value'Unit => Unit
       | (ValueStackExpOps.Value'Tuple (value'4, value'5)) =>
           (Tuple ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'5 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Value'InjL ((typ'6, typ'7), value'8)) =>
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
                  end
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'8 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Value'InjR ((typ'9, typ'10), value'11)) =>
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
                              end) typ'9 vars)
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
                              end) typ'10 vars)
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
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'11 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Value'Fun
            ((typ'12, typ'13), ((value'14, value'15), exp'16))) =>
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
                              end) typ'12 vars)
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
                              end) typ'13 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val x = (Temp.new value'14)
                                           in (x, [x])
                                           end
                        val (t1, vars'1) = let val x = (Temp.new value'15)
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
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'16 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Value'Lam ((value'17, typ'18), exp'19)) =>
           (Lam ((fn (x, _) => x)
              let
                val (t, vars') =
                  let
                    val (t0, vars'0) = let val x = (Temp.new value'17)
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
                              end) typ'18 vars)
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
                              (Abt.out ValueStackExpOps.exp_oper_unbind
                                 (Abt.unbind ValueStackExpOps.exp_oper_unbind x
                                    ~1 acc))
                          in
                            acc'
                          end) exp'19 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end))
       | ValueStackExpOps.Value'Zero => Zero
       | (ValueStackExpOps.Value'Succ value'20) =>
           (Succ ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ValueStackExpOps.value_oper_unbind
                             (Abt.unbind ValueStackExpOps.value_oper_unbind x ~1
                                acc))
                      in
                        acc'
                      end) value'20 vars)
              , []
              ))))

    fun view_out t =
      (case t of
         (Abt.Var x) => (Var x)
       | (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ValueStackExpOps.value_oper_bind (view_in [] v))
    fun out value =
      (view_out (Abt.out ValueStackExpOps.value_oper_unbind value))

    val Var' = (into o Var)
    val Comp' = (into o Comp)
    val Cont' = (into o Cont)
    val Unit' = (into Unit)
    val Tuple' = (into o Tuple)
    val InjL' = (into o InjL)
    val InjR' = (into o InjR)
    val Fun' = (into o Fun)
    val Lam' = (into o Lam)
    val Zero' = (into Zero)
    val Succ' = (into o Succ)
  end

  structure Stack =
  struct
    type value = ValueStackExpOps.value
    type exp = ValueStackExpOps.exp
    type valueVar = Temp.t
    type stack = ValueStackExpOps.stack
    type t = stack

    datatype view = Epsilon | Frame of stack * (valueVar * exp)

    fun view_in vars stack =
      (case stack of
         Epsilon => (Abt.Oper ValueStackExpOps.Stack'Epsilon)
       | (Frame (stack'1, (value'2, exp'3))) =>
           (Abt.Oper (ValueStackExpOps.Stack'Frame ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.stack_oper_bind
                             (Abt.Binding (x, acc)))) stack'1 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = value'2
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'3 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))))

    fun oper_view_out vars stack =
      (case stack of
         ValueStackExpOps.Stack'Epsilon => Epsilon
       | (ValueStackExpOps.Stack'Frame (stack'1, (value'2, exp'3))) =>
           (Frame ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.stack_oper_unbind
                                 (Abt.unbind ValueStackExpOps.stack_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) stack'1 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new value'2)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
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
              end)))

    fun view_out t =
      (case t of
         (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ValueStackExpOps.stack_oper_bind (view_in [] v))
    fun out stack =
      (view_out (Abt.out ValueStackExpOps.stack_oper_unbind stack))

    val Epsilon' = (into Epsilon)
    val Frame' = (into o Frame)
  end

  structure Exp =
  struct
    type value = ValueStackExpOps.value
    type stack = ValueStackExpOps.stack
    type valueVar = Temp.t
    type exp = ValueStackExpOps.exp
    type t = exp

    datatype view =
      Ret of value
    | Bind of value * (valueVar * exp)
    | Letcc of Typ.t * (valueVar * exp)
    | Throw of Typ.t * value * value
    | Split of value * ((valueVar * valueVar) * exp)
    | Abort of Typ.t * value
    | Case of value * (valueVar * exp) * (valueVar * exp)
    | Ap of value * value
    | Ifz of value * exp * (valueVar * exp)

    fun view_in vars exp =
      (case exp of
         (Ret value'1) =>
           (Abt.Oper (ValueStackExpOps.Exp'Ret ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ValueStackExpOps.value_oper_bind
                         (Abt.Binding (x, acc)))) value'1 vars)
              , []
              ))))
       | (Bind (value'2, (value'3, exp'4))) =>
           (Abt.Oper (ValueStackExpOps.Exp'Bind ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = value'3
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'4 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Letcc (typ'5, (value'6, exp'7))) =>
           (Abt.Oper (ValueStackExpOps.Exp'Letcc ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'5 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = value'6
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'7 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Throw (typ'8, value'9, value'10)) =>
           (Abt.Oper (ValueStackExpOps.Exp'Throw ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'8 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'9 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'10 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Split (value'11, ((value'12, value'13), exp'14))) =>
           (Abt.Oper (ValueStackExpOps.Exp'Split ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'11 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val var = value'12
                                           in ((Temp.toUserString var), [var])
                                           end
                        val (t1, vars'1) = let val var = value'13
                                           in ((Temp.toUserString var), [var])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'14 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Abort (typ'15, value'16)) =>
           (Abt.Oper (ValueStackExpOps.Exp'Abort ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'15 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'16 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Case (value'17, (value'18, exp'19), (value'20, exp'21))) =>
           (Abt.Oper (ValueStackExpOps.Exp'Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'17 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = value'18
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'19 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = value'20
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'21 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Ap (value'22, value'23)) =>
           (Abt.Oper (ValueStackExpOps.Exp'Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'22 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'23 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Ifz (value'24, exp'25, (value'26, exp'27))) =>
           (Abt.Oper (ValueStackExpOps.Exp'Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.value_oper_bind
                             (Abt.Binding (x, acc)))) value'24 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ValueStackExpOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'25 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = value'26
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ValueStackExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'27 vars)
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
         (ValueStackExpOps.Exp'Ret value'1) =>
           (Ret ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ValueStackExpOps.value_oper_unbind
                             (Abt.unbind ValueStackExpOps.value_oper_unbind x ~1
                                acc))
                      in
                        acc'
                      end) value'1 vars)
              , []
              )))
       | (ValueStackExpOps.Exp'Bind (value'2, (value'3, exp'4))) =>
           (Bind ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new value'3)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'4 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Exp'Letcc (typ'5, (value'6, exp'7))) =>
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
                          end) typ'5 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new value'6)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'7 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Exp'Throw (typ'8, value'9, value'10)) =>
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
                          end) typ'8 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'9 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'10 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ValueStackExpOps.Exp'Split (value'11, ((value'12, value'13), exp'14))) =>
           (Split ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'11 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val x = (Temp.new value'12)
                                           in (x, [x])
                                           end
                        val (t1, vars'1) = let val x = (Temp.new value'13)
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
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'14 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Exp'Abort (typ'15, value'16)) =>
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
                          end) typ'15 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'16 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Exp'Case
            (value'17, (value'18, exp'19), (value'20, exp'21))) =>
           (Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'17 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new value'18)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'19 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new value'20)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'21 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ValueStackExpOps.Exp'Ap (value'22, value'23)) =>
           (Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'22 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'23 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ValueStackExpOps.Exp'Ifz (value'24, exp'25, (value'26, exp'27))) =>
           (Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.value_oper_unbind
                                 (Abt.unbind ValueStackExpOps.value_oper_unbind
                                    x ~1 acc))
                          in
                            acc'
                          end) value'24 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ValueStackExpOps.exp_oper_unbind
                                 (Abt.unbind ValueStackExpOps.exp_oper_unbind x
                                    ~1 acc))
                          in
                            acc'
                          end) exp'25 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new value'26)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ValueStackExpOps.exp_oper_unbind
                                     (Abt.unbind
                                        ValueStackExpOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'27 vars)
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
         (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ValueStackExpOps.exp_oper_bind (view_in [] v))
    fun out exp =
      (view_out (Abt.out ValueStackExpOps.exp_oper_unbind exp))

    val Ret' = (into o Ret)
    val Bind' = (into o Bind)
    val Letcc' = (into o Letcc)
    val Throw' = (into o Throw)
    val Split' = (into o Split)
    val Abort' = (into o Abort)
    val Case' = (into o Case)
    val Ap' = (into o Ap)
    val Ifz' = (into o Ifz)
  end

  fun value_toString value =
    (case (Value.out value) of
       (Value.Var x) => (Value.Var.toString x)
     | (Value.Comp exp'1) => ("(Comp " ^ (exp_toString exp'1) ^ ")")
     | (Value.Cont (typ'2, stack'3)) =>
         ("(Cont "
          ^ ("(" ^ (Typ.toString typ'2) ^ ", " ^ (stack_toString stack'3) ^ ")")
          ^ ")")
     | Value.Unit => "Unit"
     | (Value.Tuple (value'4, value'5)) =>
         ("(Tuple "
          ^
          ("(" ^ (value_toString value'4) ^ ", " ^ (value_toString value'5)
           ^ ")") ^ ")")
     | (Value.InjL ((typ'6, typ'7), value'8)) =>
         ("(InjL "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'6) ^ ", " ^ (Typ.toString typ'7) ^ ")")
           ^ ", " ^ (value_toString value'8) ^ ")") ^ ")")
     | (Value.InjR ((typ'9, typ'10), value'11)) =>
         ("(InjR "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'9) ^ ", " ^ (Typ.toString typ'10) ^ ")")
           ^ ", " ^ (value_toString value'11) ^ ")") ^ ")")
     | (Value.Fun ((typ'12, typ'13), ((value'14, value'15), exp'16))) =>
         ("(Fun "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'12) ^ ", " ^ (Typ.toString typ'13) ^ ")")
           ^ ", "
           ^
           ("("
            ^
            ("(" ^ (Value.Var.toString value'14) ^ ", "
             ^ (Value.Var.toString value'15) ^ ")") ^ " . "
            ^ (exp_toString exp'16) ^ ")") ^ ")") ^ ")")
     | (Value.Lam ((value'17, typ'18), exp'19)) =>
         ("(Lam "
          ^
          ("("
           ^
           ("(" ^ (Value.Var.toString value'17) ^ ", " ^ (Typ.toString typ'18)
            ^ ")") ^ " . " ^ (exp_toString exp'19) ^ ")") ^ ")")
     | Value.Zero => "Zero"
     | (Value.Succ value'20) => ("(Succ " ^ (value_toString value'20) ^ ")"))
  and stack_toString stack =
    (case (Stack.out stack) of
       Stack.Epsilon => "Epsilon"
     | (Stack.Frame (stack'1, (value'2, exp'3))) =>
         ("(Frame "
          ^
          ("(" ^ (stack_toString stack'1) ^ ", "
           ^
           ("(" ^ (Value.Var.toString value'2) ^ " . " ^ (exp_toString exp'3)
            ^ ")") ^ ")") ^ ")"))
  and exp_toString exp =
    (case (Exp.out exp) of
       (Exp.Ret value'1) => ("(Ret " ^ (value_toString value'1) ^ ")")
     | (Exp.Bind (value'2, (value'3, exp'4))) =>
         ("(Bind "
          ^
          ("(" ^ (value_toString value'2) ^ ", "
           ^
           ("(" ^ (Value.Var.toString value'3) ^ " . " ^ (exp_toString exp'4)
            ^ ")") ^ ")") ^ ")")
     | (Exp.Letcc (typ'5, (value'6, exp'7))) =>
         ("(Letcc "
          ^
          ("(" ^ (Typ.toString typ'5) ^ ", "
           ^
           ("(" ^ (Value.Var.toString value'6) ^ " . " ^ (exp_toString exp'7)
            ^ ")") ^ ")") ^ ")")
     | (Exp.Throw (typ'8, value'9, value'10)) =>
         ("(Throw "
          ^
          ("(" ^ (Typ.toString typ'8) ^ ", " ^ (value_toString value'9) ^ ", "
           ^ (value_toString value'10) ^ ")") ^ ")")
     | (Exp.Split (value'11, ((value'12, value'13), exp'14))) =>
         ("(Split "
          ^
          ("(" ^ (value_toString value'11) ^ ", "
           ^
           ("("
            ^
            ("(" ^ (Value.Var.toString value'12) ^ ", "
             ^ (Value.Var.toString value'13) ^ ")") ^ " . "
            ^ (exp_toString exp'14) ^ ")") ^ ")") ^ ")")
     | (Exp.Abort (typ'15, value'16)) =>
         ("(Abort "
          ^
          ("(" ^ (Typ.toString typ'15) ^ ", " ^ (value_toString value'16) ^ ")")
          ^ ")")
     | (Exp.Case (value'17, (value'18, exp'19), (value'20, exp'21))) =>
         ("(Case "
          ^
          ("(" ^ (value_toString value'17) ^ ", "
           ^
           ("(" ^ (Value.Var.toString value'18) ^ " . " ^ (exp_toString exp'19)
            ^ ")") ^ ", "
           ^
           ("(" ^ (Value.Var.toString value'20) ^ " . " ^ (exp_toString exp'21)
            ^ ")") ^ ")") ^ ")")
     | (Exp.Ap (value'22, value'23)) =>
         ("(Ap "
          ^
          ("(" ^ (value_toString value'22) ^ ", " ^ (value_toString value'23)
           ^ ")") ^ ")")
     | (Exp.Ifz (value'24, exp'25, (value'26, exp'27))) =>
         ("(Ifz "
          ^
          ("(" ^ (value_toString value'24) ^ ", " ^ (exp_toString exp'25) ^ ", "
           ^
           ("(" ^ (Value.Var.toString value'26) ^ " . " ^ (exp_toString exp'27)
            ^ ")") ^ ")") ^ ")"))

  fun value_subst t x value =
    (case (Value.out value) of
       (Value.Var y) => (if (Value.Var.equal (x, y)) then t else (Value.Var' y))
     | (Value.Comp exp'1) => (Value.Comp' (exp_substValue t x exp'1))
     | (Value.Cont (typ'2, stack'3)) =>
         (Value.Cont' (typ'2, (stack_substValue t x stack'3)))
     | Value.Unit => Value.Unit'
     | (Value.Tuple (value'4, value'5)) =>
         (Value.Tuple' ((value_subst t x value'4), (value_subst t x value'5)))
     | (Value.InjL ((typ'6, typ'7), value'8)) =>
         (Value.InjL' ((typ'6, typ'7), (value_subst t x value'8)))
     | (Value.InjR ((typ'9, typ'10), value'11)) =>
         (Value.InjR' ((typ'9, typ'10), (value_subst t x value'11)))
     | (Value.Fun ((typ'12, typ'13), ((value'14, value'15), exp'16))) =>
         (Value.Fun'
            ( (typ'12, typ'13)
            , ((value'14, value'15), (exp_substValue t x exp'16))
            ))
     | (Value.Lam ((value'17, typ'18), exp'19)) =>
         (Value.Lam' ((value'17, typ'18), (exp_substValue t x exp'19)))
     | Value.Zero => Value.Zero'
     | (Value.Succ value'20) => (Value.Succ' (value_subst t x value'20)))
  and stack_substValue t x stack =
    (case (Stack.out stack) of
       Stack.Epsilon => Stack.Epsilon'
     | (Stack.Frame (stack'1, (value'2, exp'3))) =>
         (Stack.Frame'
            ( (stack_substValue t x stack'1)
            , (value'2, (exp_substValue t x exp'3))
            )))
  and exp_substValue t x exp =
    (case (Exp.out exp) of
       (Exp.Ret value'1) => (Exp.Ret' (value_subst t x value'1))
     | (Exp.Bind (value'2, (value'3, exp'4))) =>
         (Exp.Bind'
            ((value_subst t x value'2), (value'3, (exp_substValue t x exp'4))))
     | (Exp.Letcc (typ'5, (value'6, exp'7))) =>
         (Exp.Letcc' (typ'5, (value'6, (exp_substValue t x exp'7))))
     | (Exp.Throw (typ'8, value'9, value'10)) =>
         (Exp.Throw'
            (typ'8, (value_subst t x value'9), (value_subst t x value'10)))
     | (Exp.Split (value'11, ((value'12, value'13), exp'14))) =>
         (Exp.Split'
            ( (value_subst t x value'11)
            , ((value'12, value'13), (exp_substValue t x exp'14))
            ))
     | (Exp.Abort (typ'15, value'16)) =>
         (Exp.Abort' (typ'15, (value_subst t x value'16)))
     | (Exp.Case (value'17, (value'18, exp'19), (value'20, exp'21))) =>
         (Exp.Case'
            ( (value_subst t x value'17)
            , (value'18, (exp_substValue t x exp'19))
            , (value'20, (exp_substValue t x exp'21))
            ))
     | (Exp.Ap (value'22, value'23)) =>
         (Exp.Ap' ((value_subst t x value'22), (value_subst t x value'23)))
     | (Exp.Ifz (value'24, exp'25, (value'26, exp'27))) =>
         (Exp.Ifz'
            ( (value_subst t x value'24)
            , (exp_substValue t x exp'25)
            , (value'26, (exp_substValue t x exp'27))
            )))

  structure Value =
  struct
    open Value
    val toString = value_toString
    val aequiv = (Abt.aequiv ValueStackExpOps.value_oper_aequiv)
    val subst = value_subst
  end

  structure Stack =
  struct
    open Stack
    val toString = stack_toString
    val aequiv = (Abt.aequiv ValueStackExpOps.stack_oper_aequiv)
    val substValue = stack_substValue
  end

  structure Exp =
  struct
    open Exp
    val toString = exp_toString
    val aequiv = (Abt.aequiv ValueStackExpOps.exp_oper_aequiv)
    val substValue = exp_substValue
  end

  structure StateOps =
  struct
    datatype state =
      State'Eval of Stack.t * Exp.t
    | State'Ret of Stack.t * Value.t

    fun state_bind x i state =
      (case state of
         (State'Eval (stack'1, exp'2)) =>
           (State'Eval
              ( (Abt.bind ValueStackExpOps.stack_oper_bind x i stack'1)
              , (Abt.bind ValueStackExpOps.exp_oper_bind x i exp'2)
              ))
       | (State'Ret (stack'3, value'4)) =>
           (State'Ret
              ( (Abt.bind ValueStackExpOps.stack_oper_bind x i stack'3)
              , (Abt.bind ValueStackExpOps.value_oper_bind x i value'4)
              )))

    fun state_unbind x i state =
      (case state of
         (State'Eval (stack'1, exp'2)) =>
           (State'Eval
              ( (Abt.unbind ValueStackExpOps.stack_oper_unbind x i stack'1)
              , (Abt.unbind ValueStackExpOps.exp_oper_unbind x i exp'2)
              ))
       | (State'Ret (stack'3, value'4)) =>
           (State'Ret
              ( (Abt.unbind ValueStackExpOps.stack_oper_unbind x i stack'3)
              , (Abt.unbind ValueStackExpOps.value_oper_unbind x i value'4)
              )))

    fun state_internal_aequiv (state1, state2) =
      (case (state1, state2) of
         ((State'Eval (stack'1, exp'3)), (State'Eval (stack'2, exp'4))) =>
           ((Stack.aequiv (stack'1, stack'2))
            andalso (Exp.aequiv (exp'3, exp'4)))
       | ((State'Ret (stack'1, value'3)), (State'Ret (stack'2, value'4))) =>
           ((Stack.aequiv (stack'1, stack'2))
            andalso (Value.aequiv (value'3, value'4)))
       | _ => false)
  end

  datatype state = Eval of Stack.t * Exp.t | Ret of Stack.t * Value.t

  fun state_view_in vars state =
    (case state of
       (Eval (stack'1, exp'2)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) =
                 ( (List.foldl
                      (fn (x, acc) =>
                         (Abt.into ValueStackExpOps.stack_oper_bind
                            (Abt.Binding (x, acc)))) stack'1 vars)
                 , []
                 )
               val (t1, vars'1) =
                 ( (List.foldl
                      (fn (x, acc) =>
                         (Abt.into ValueStackExpOps.exp_oper_bind
                            (Abt.Binding (x, acc)))) exp'2 vars)
                 , []
                 )
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((StateOps.State'Eval t), vars')
         end
     | (Ret (stack'3, value'4)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) =
                 ( (List.foldl
                      (fn (x, acc) =>
                         (Abt.into ValueStackExpOps.stack_oper_bind
                            (Abt.Binding (x, acc)))) stack'3 vars)
                 , []
                 )
               val (t1, vars'1) =
                 ( (List.foldl
                      (fn (x, acc) =>
                         (Abt.into ValueStackExpOps.value_oper_bind
                            (Abt.Binding (x, acc)))) value'4 vars)
                 , []
                 )
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((StateOps.State'Ret t), vars')
         end)

  fun state_view_out vars state =
    (case state of
       (StateOps.State'Eval (stack'1, exp'2)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) =
                 ( (List.foldr
                      (fn (x, acc) =>
                         let
                           val (Abt.Binding (_, acc')) =
                             (Abt.out ValueStackExpOps.stack_oper_unbind
                                (Abt.unbind ValueStackExpOps.stack_oper_unbind x
                                   ~1 acc))
                         in
                           acc'
                         end) stack'1 vars)
                 , []
                 )
               val (t1, vars'1) =
                 ( (List.foldr
                      (fn (x, acc) =>
                         let
                           val (Abt.Binding (_, acc')) =
                             (Abt.out ValueStackExpOps.exp_oper_unbind
                                (Abt.unbind ValueStackExpOps.exp_oper_unbind x
                                   ~1 acc))
                         in
                           acc'
                         end) exp'2 vars)
                 , []
                 )
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((Eval t), vars')
         end
     | (StateOps.State'Ret (stack'3, value'4)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) =
                 ( (List.foldr
                      (fn (x, acc) =>
                         let
                           val (Abt.Binding (_, acc')) =
                             (Abt.out ValueStackExpOps.stack_oper_unbind
                                (Abt.unbind ValueStackExpOps.stack_oper_unbind x
                                   ~1 acc))
                         in
                           acc'
                         end) stack'3 vars)
                 , []
                 )
               val (t1, vars'1) =
                 ( (List.foldr
                      (fn (x, acc) =>
                         let
                           val (Abt.Binding (_, acc')) =
                             (Abt.out ValueStackExpOps.value_oper_unbind
                                (Abt.unbind ValueStackExpOps.value_oper_unbind x
                                   ~1 acc))
                         in
                           acc'
                         end) value'4 vars)
                 , []
                 )
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((Ret t), vars')
         end)

  structure State = struct datatype state = datatype state type t = state end

  fun state_aequiv (state1, state2) =
    (case (state1, state2) of
       ((Eval (stack'1, exp'3)), (Eval (stack'2, exp'4))) =>
         ((Stack.aequiv (stack'1, stack'2)) andalso (Exp.aequiv (exp'3, exp'4)))
     | ((Ret (stack'1, value'3)), (Ret (stack'2, value'4))) =>
         ((Stack.aequiv (stack'1, stack'2))
          andalso (Value.aequiv (value'3, value'4)))
     | _ => false)

  fun state_toString state =
    (case state of
       (State.Eval (stack'1, exp'2)) =>
         ("(Eval "
          ^ ("(" ^ (Stack.toString stack'1) ^ ", " ^ (Exp.toString exp'2) ^ ")")
          ^ ")")
     | (State.Ret (stack'3, value'4)) =>
         ("(Ret "
          ^
          ("(" ^ (Stack.toString stack'3) ^ ", " ^ (Value.toString value'4)
           ^ ")") ^ ")"))

  fun state_substValue t x state =
    (case state of
       (State.Eval (stack'1, exp'2)) =>
         (State.Eval
            ((Stack.substValue t x stack'1), (Exp.substValue t x exp'2)))
     | (State.Ret (stack'3, value'4)) =>
         (State.Ret ((Stack.substValue t x stack'3), (Value.subst t x value'4))))

  structure State =
  struct
    open State
    val toString = state_toString
    val internal_aequiv = StateOps.state_internal_aequiv
    val aequiv = state_aequiv
    val substValue = state_substValue
  end
end

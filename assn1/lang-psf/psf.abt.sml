structure PSF :> PSF =
struct

  structure TypOps =
  struct
    datatype typ =
      Typ'Arrow of typ * typ
    | Typ'Unit
    | Typ'Prod of typ * typ
    | Typ'Void
    | Typ'Sum of typ * typ
    | Typ'A
    | Typ'B
    | Typ'C

    fun typ_bind x i typ =
      (case typ of
         (Typ'Arrow (typ'1, typ'2)) =>
           (Typ'Arrow ((typ_bind x i typ'1), (typ_bind x i typ'2)))
       | Typ'Unit => Typ'Unit
       | (Typ'Prod (typ'3, typ'4)) =>
           (Typ'Prod ((typ_bind x i typ'3), (typ_bind x i typ'4)))
       | Typ'Void => Typ'Void
       | (Typ'Sum (typ'5, typ'6)) =>
           (Typ'Sum ((typ_bind x i typ'5), (typ_bind x i typ'6)))
       | Typ'A => Typ'A
       | Typ'B => Typ'B
       | Typ'C => Typ'C)

    fun typ_unbind x i typ =
      (case typ of
         (Typ'Arrow (typ'1, typ'2)) =>
           (Typ'Arrow ((typ_unbind x i typ'1), (typ_unbind x i typ'2)))
       | Typ'Unit => Typ'Unit
       | (Typ'Prod (typ'3, typ'4)) =>
           (Typ'Prod ((typ_unbind x i typ'3), (typ_unbind x i typ'4)))
       | Typ'Void => Typ'Void
       | (Typ'Sum (typ'5, typ'6)) =>
           (Typ'Sum ((typ_unbind x i typ'5), (typ_unbind x i typ'6)))
       | Typ'A => Typ'A
       | Typ'B => Typ'B
       | Typ'C => Typ'C)

    fun typ_internal_aequiv (typ1, typ2) =
      (case (typ1, typ2) of
         ((Typ'Arrow (typ'1, typ'3)), (Typ'Arrow (typ'2, typ'4))) =>
           ((typ_internal_aequiv (typ'1, typ'2))
            andalso (typ_internal_aequiv (typ'3, typ'4)))
       | (Typ'Unit, Typ'Unit) => true
       | ((Typ'Prod (typ'1, typ'3)), (Typ'Prod (typ'2, typ'4))) =>
           ((typ_internal_aequiv (typ'1, typ'2))
            andalso (typ_internal_aequiv (typ'3, typ'4)))
       | (Typ'Void, Typ'Void) => true
       | ((Typ'Sum (typ'1, typ'3)), (Typ'Sum (typ'2, typ'4))) =>
           ((typ_internal_aequiv (typ'1, typ'2))
            andalso (typ_internal_aequiv (typ'3, typ'4)))
       | (Typ'A, Typ'A) => true
       | (Typ'B, Typ'B) => true
       | (Typ'C, Typ'C) => true
       | _ => false)
  end

  datatype typ =
    Arrow of typ * typ
  | Unit
  | Prod of typ * typ
  | Void
  | Sum of typ * typ
  | A
  | B
  | C

  fun typ_view_in vars typ =
    (case typ of
       (Arrow (typ'1, typ'2)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) = (typ_view_in vars typ'1)
               val (t1, vars'1) = (typ_view_in vars typ'2)
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((TypOps.Typ'Arrow t), vars')
         end
     | Unit => (TypOps.Typ'Unit, [])
     | (Prod (typ'3, typ'4)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) = (typ_view_in vars typ'3)
               val (t1, vars'1) = (typ_view_in vars typ'4)
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((TypOps.Typ'Prod t), vars')
         end
     | Void => (TypOps.Typ'Void, [])
     | (Sum (typ'5, typ'6)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) = (typ_view_in vars typ'5)
               val (t1, vars'1) = (typ_view_in vars typ'6)
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((TypOps.Typ'Sum t), vars')
         end
     | A => (TypOps.Typ'A, [])
     | B => (TypOps.Typ'B, [])
     | C => (TypOps.Typ'C, []))

  fun typ_view_out vars typ =
    (case typ of
       (TypOps.Typ'Arrow (typ'1, typ'2)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) = (typ_view_out vars typ'1)
               val (t1, vars'1) = (typ_view_out vars typ'2)
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((Arrow t), vars')
         end
     | TypOps.Typ'Unit => (Unit, [])
     | (TypOps.Typ'Prod (typ'3, typ'4)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) = (typ_view_out vars typ'3)
               val (t1, vars'1) = (typ_view_out vars typ'4)
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((Prod t), vars')
         end
     | TypOps.Typ'Void => (Void, [])
     | (TypOps.Typ'Sum (typ'5, typ'6)) =>
         let
           val (t, vars') =
             let
               val (t0, vars'0) = (typ_view_out vars typ'5)
               val (t1, vars'1) = (typ_view_out vars typ'6)
             in
               ((t0, t1), (vars'0 @ vars'1))
             end
         in
           ((Sum t), vars')
         end
     | TypOps.Typ'A => (A, [])
     | TypOps.Typ'B => (B, [])
     | TypOps.Typ'C => (C, []))

  structure Typ = struct datatype typ = datatype typ type t = typ end

  fun typ_aequiv (typ1, typ2) =
    (case (typ1, typ2) of
       ((Arrow (typ'1, typ'3)), (Arrow (typ'2, typ'4))) =>
         ((typ_aequiv (typ'1, typ'2)) andalso (typ_aequiv (typ'3, typ'4)))
     | (Unit, Unit) => true
     | ((Prod (typ'1, typ'3)), (Prod (typ'2, typ'4))) =>
         ((typ_aequiv (typ'1, typ'2)) andalso (typ_aequiv (typ'3, typ'4)))
     | (Void, Void) => true
     | ((Sum (typ'1, typ'3)), (Sum (typ'2, typ'4))) =>
         ((typ_aequiv (typ'1, typ'2)) andalso (typ_aequiv (typ'3, typ'4)))
     | (A, A) => true
     | (B, B) => true
     | (C, C) => true
     | _ => false)

  fun typ_toString typ =
    (case typ of
       (Typ.Arrow (typ'1, typ'2)) =>
         ("(Arrow "
          ^ ("(" ^ (typ_toString typ'1) ^ ", " ^ (typ_toString typ'2) ^ ")")
          ^ ")")
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
     | Typ.A => "A"
     | Typ.B => "B"
     | Typ.C => "C")

  structure Typ =
  struct
    open Typ
    val toString = typ_toString
    val internal_aequiv = TypOps.typ_internal_aequiv
    val aequiv = typ_aequiv
  end

  structure ExpOps =
  struct
    datatype exp_oper =
      Exp'Lam of TypOps.typ * (string * exp)
    | Exp'Ap of exp * exp
    | Exp'Triv
    | Exp'Pair of exp * exp
    | Exp'PrL of exp
    | Exp'PrR of exp
    | Exp'Abort of TypOps.typ * exp
    | Exp'InL of (TypOps.typ * TypOps.typ) * exp
    | Exp'InR of (TypOps.typ * TypOps.typ) * exp
    | Exp'Case of TypOps.typ * exp * (string * exp) * (string * exp)
    withtype exp = exp_oper Abt.t

    fun exp_oper_bind x i exp =
      (case exp of
         (Exp'Lam (typ'1, (exp'2, exp'3))) =>
           (Exp'Lam
              ( (TypOps.typ_bind x i typ'1)
              , (exp'2, (Abt.bind exp_oper_bind x i exp'3))
              ))
       | (Exp'Ap (exp'4, exp'5)) =>
           (Exp'Ap
              ( (Abt.bind exp_oper_bind x i exp'4)
              , (Abt.bind exp_oper_bind x i exp'5)
              ))
       | Exp'Triv => Exp'Triv
       | (Exp'Pair (exp'6, exp'7)) =>
           (Exp'Pair
              ( (Abt.bind exp_oper_bind x i exp'6)
              , (Abt.bind exp_oper_bind x i exp'7)
              ))
       | (Exp'PrL exp'8) => (Exp'PrL (Abt.bind exp_oper_bind x i exp'8))
       | (Exp'PrR exp'9) => (Exp'PrR (Abt.bind exp_oper_bind x i exp'9))
       | (Exp'Abort (typ'10, exp'11)) =>
           (Exp'Abort
              ( (TypOps.typ_bind x i typ'10)
              , (Abt.bind exp_oper_bind x i exp'11)
              ))
       | (Exp'InL ((typ'12, typ'13), exp'14)) =>
           (Exp'InL
              ( ((TypOps.typ_bind x i typ'12), (TypOps.typ_bind x i typ'13))
              , (Abt.bind exp_oper_bind x i exp'14)
              ))
       | (Exp'InR ((typ'15, typ'16), exp'17)) =>
           (Exp'InR
              ( ((TypOps.typ_bind x i typ'15), (TypOps.typ_bind x i typ'16))
              , (Abt.bind exp_oper_bind x i exp'17)
              ))
       | (Exp'Case (typ'18, exp'19, (exp'20, exp'21), (exp'22, exp'23))) =>
           (Exp'Case
              ( (TypOps.typ_bind x i typ'18)
              , (Abt.bind exp_oper_bind x i exp'19)
              , (exp'20, (Abt.bind exp_oper_bind x i exp'21))
              , (exp'22, (Abt.bind exp_oper_bind x i exp'23))
              )))

    fun exp_oper_unbind x i exp =
      (case exp of
         (Exp'Lam (typ'1, (exp'2, exp'3))) =>
           (Exp'Lam
              ( (TypOps.typ_unbind x i typ'1)
              , (exp'2, (Abt.unbind exp_oper_unbind x i exp'3))
              ))
       | (Exp'Ap (exp'4, exp'5)) =>
           (Exp'Ap
              ( (Abt.unbind exp_oper_unbind x i exp'4)
              , (Abt.unbind exp_oper_unbind x i exp'5)
              ))
       | Exp'Triv => Exp'Triv
       | (Exp'Pair (exp'6, exp'7)) =>
           (Exp'Pair
              ( (Abt.unbind exp_oper_unbind x i exp'6)
              , (Abt.unbind exp_oper_unbind x i exp'7)
              ))
       | (Exp'PrL exp'8) => (Exp'PrL (Abt.unbind exp_oper_unbind x i exp'8))
       | (Exp'PrR exp'9) => (Exp'PrR (Abt.unbind exp_oper_unbind x i exp'9))
       | (Exp'Abort (typ'10, exp'11)) =>
           (Exp'Abort
              ( (TypOps.typ_unbind x i typ'10)
              , (Abt.unbind exp_oper_unbind x i exp'11)
              ))
       | (Exp'InL ((typ'12, typ'13), exp'14)) =>
           (Exp'InL
              ( ((TypOps.typ_unbind x i typ'12), (TypOps.typ_unbind x i typ'13))
              , (Abt.unbind exp_oper_unbind x i exp'14)
              ))
       | (Exp'InR ((typ'15, typ'16), exp'17)) =>
           (Exp'InR
              ( ((TypOps.typ_unbind x i typ'15), (TypOps.typ_unbind x i typ'16))
              , (Abt.unbind exp_oper_unbind x i exp'17)
              ))
       | (Exp'Case (typ'18, exp'19, (exp'20, exp'21), (exp'22, exp'23))) =>
           (Exp'Case
              ( (TypOps.typ_unbind x i typ'18)
              , (Abt.unbind exp_oper_unbind x i exp'19)
              , (exp'20, (Abt.unbind exp_oper_unbind x i exp'21))
              , (exp'22, (Abt.unbind exp_oper_unbind x i exp'23))
              )))

    fun exp_oper_aequiv (exp1, exp2) =
      (case (exp1, exp2) of
         ((Exp'Lam (typ'1, (_, exp'3))), (Exp'Lam (typ'2, (_, exp'4)))) =>
           ((Typ.internal_aequiv (typ'1, typ'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Ap (exp'1, exp'3)), (Exp'Ap (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | (Exp'Triv, Exp'Triv) => true
       | ((Exp'Pair (exp'1, exp'3)), (Exp'Pair (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'PrL exp'1), (Exp'PrL exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Exp'PrR exp'1), (Exp'PrR exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Exp'Abort (typ'1, exp'3)), (Exp'Abort (typ'2, exp'4))) =>
           ((Typ.internal_aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'InL ((typ'1, typ'3), exp'5)), (Exp'InL ((typ'2, typ'4), exp'6))) =>
           (((Typ.internal_aequiv (typ'1, typ'2))
             andalso (Typ.internal_aequiv (typ'3, typ'4)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
       | ((Exp'InR ((typ'1, typ'3), exp'5)), (Exp'InR ((typ'2, typ'4), exp'6))) =>
           (((Typ.internal_aequiv (typ'1, typ'2))
             andalso (Typ.internal_aequiv (typ'3, typ'4)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
       | ( (Exp'Case (typ'1, exp'3, (_, exp'5), (_, exp'7)))
         , (Exp'Case (typ'2, exp'4, (_, exp'6), (_, exp'8)))
         ) =>
           ((Typ.internal_aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'7, exp'8))))
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
    | Lam of Typ.t * (expVar * exp)
    | Ap of exp * exp
    | Triv
    | Pair of exp * exp
    | PrL of exp
    | PrR of exp
    | Abort of Typ.t * exp
    | InL of (Typ.t * Typ.t) * exp
    | InR of (Typ.t * Typ.t) * exp
    | Case of Typ.t * exp * (expVar * exp) * (expVar * exp)

    fun view_in vars exp =
      (case exp of
         (Var x) => (Abt.Var x)
       | (Lam (typ'1, (exp'2, exp'3))) =>
           (Abt.Oper (ExpOps.Exp'Lam ((fn (x, _) => x)
              let
                val (t0, vars'0) = (typ_view_in vars typ'1)
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
       | (Ap (exp'4, exp'5)) =>
           (Abt.Oper (ExpOps.Exp'Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'5 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Triv => (Abt.Oper ExpOps.Exp'Triv)
       | (Pair (exp'6, exp'7)) =>
           (Abt.Oper (ExpOps.Exp'Pair ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'6 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (PrL exp'8) =>
           (Abt.Oper (ExpOps.Exp'PrL ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'8 vars)
              , []
              ))))
       | (PrR exp'9) =>
           (Abt.Oper (ExpOps.Exp'PrR ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'9 vars)
              , []
              ))))
       | (Abort (typ'10, exp'11)) =>
           (Abt.Oper (ExpOps.Exp'Abort ((fn (x, _) => x)
              let
                val (t0, vars'0) = (typ_view_in vars typ'10)
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
       | (InL ((typ'12, typ'13), exp'14)) =>
           (Abt.Oper (ExpOps.Exp'InL ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) = (typ_view_in vars typ'12)
                    val (t1, vars'1) = (typ_view_in vars typ'13)
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'14 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (InR ((typ'15, typ'16), exp'17)) =>
           (Abt.Oper (ExpOps.Exp'InR ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) = (typ_view_in vars typ'15)
                    val (t1, vars'1) = (typ_view_in vars typ'16)
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
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
       | (Case (typ'18, exp'19, (exp'20, exp'21), (exp'22, exp'23))) =>
           (Abt.Oper (ExpOps.Exp'Case ((fn (x, _) => x)
              let
                val (t0, vars'0) = (typ_view_in vars typ'18)
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'19 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = exp'20
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'21 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t3, vars'3) =
                  let
                    val (t, vars') = let val var = exp'22
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'23 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2, t3), (vars'0 @ vars'1 @ vars'2 @ vars'3))
              end))))

    fun oper_view_out vars exp =
      (case exp of
         (ExpOps.Exp'Lam (typ'1, (exp'2, exp'3))) =>
           (Lam ((fn (x, _) => x)
              let
                val (t0, vars'0) = (typ_view_out vars typ'1)
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
       | (ExpOps.Exp'Ap (exp'4, exp'5)) =>
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
                          end) exp'4 vars)
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
                          end) exp'5 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | ExpOps.Exp'Triv => Triv
       | (ExpOps.Exp'Pair (exp'6, exp'7)) =>
           (Pair ((fn (x, _) => x)
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
                          end) exp'6 vars)
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
                          end) exp'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'PrL exp'8) =>
           (PrL ((fn (x, _) => x)
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
              )))
       | (ExpOps.Exp'PrR exp'9) =>
           (PrR ((fn (x, _) => x)
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
              )))
       | (ExpOps.Exp'Abort (typ'10, exp'11)) =>
           (Abort ((fn (x, _) => x)
              let
                val (t0, vars'0) = (typ_view_out vars typ'10)
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
       | (ExpOps.Exp'InL ((typ'12, typ'13), exp'14)) =>
           (InL ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) = (typ_view_out vars typ'12)
                    val (t1, vars'1) = (typ_view_out vars typ'13)
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
                          end) exp'14 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'InR ((typ'15, typ'16), exp'17)) =>
           (InR ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) = (typ_view_out vars typ'15)
                    val (t1, vars'1) = (typ_view_out vars typ'16)
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
                          end) exp'17 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Case (typ'18, exp'19, (exp'20, exp'21), (exp'22, exp'23))) =>
           (Case ((fn (x, _) => x)
              let
                val (t0, vars'0) = (typ_view_out vars typ'18)
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'19 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'20)
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
                              end) exp'21 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t3, vars'3) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'22)
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
                              end) exp'23 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2, t3), (vars'0 @ vars'1 @ vars'2 @ vars'3))
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
    val Lam' = (into o Lam)
    val Ap' = (into o Ap)
    val Triv' = (into Triv)
    val Pair' = (into o Pair)
    val PrL' = (into o PrL)
    val PrR' = (into o PrR)
    val Abort' = (into o Abort)
    val InL' = (into o InL)
    val InR' = (into o InR)
    val Case' = (into o Case)
  end

  fun exp_toString exp =
    (case (Exp.out exp) of
       (Exp.Var x) => (Exp.Var.toString x)
     | (Exp.Lam (typ'1, (exp'2, exp'3))) =>
         ("(Lam "
          ^
          ("(" ^ (Typ.toString typ'1) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'2) ^ " . " ^ (exp_toString exp'3) ^ ")")
           ^ ")") ^ ")")
     | (Exp.Ap (exp'4, exp'5)) =>
         ("(Ap "
          ^ ("(" ^ (exp_toString exp'4) ^ ", " ^ (exp_toString exp'5) ^ ")")
          ^ ")")
     | Exp.Triv => "Triv"
     | (Exp.Pair (exp'6, exp'7)) =>
         ("(Pair "
          ^ ("(" ^ (exp_toString exp'6) ^ ", " ^ (exp_toString exp'7) ^ ")")
          ^ ")")
     | (Exp.PrL exp'8) => ("(PrL " ^ (exp_toString exp'8) ^ ")")
     | (Exp.PrR exp'9) => ("(PrR " ^ (exp_toString exp'9) ^ ")")
     | (Exp.Abort (typ'10, exp'11)) =>
         ("(Abort "
          ^ ("(" ^ (Typ.toString typ'10) ^ ", " ^ (exp_toString exp'11) ^ ")")
          ^ ")")
     | (Exp.InL ((typ'12, typ'13), exp'14)) =>
         ("(InL "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'12) ^ ", " ^ (Typ.toString typ'13) ^ ")")
           ^ ", " ^ (exp_toString exp'14) ^ ")") ^ ")")
     | (Exp.InR ((typ'15, typ'16), exp'17)) =>
         ("(InR "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'15) ^ ", " ^ (Typ.toString typ'16) ^ ")")
           ^ ", " ^ (exp_toString exp'17) ^ ")") ^ ")")
     | (Exp.Case (typ'18, exp'19, (exp'20, exp'21), (exp'22, exp'23))) =>
         ("(Case "
          ^
          ("(" ^ (Typ.toString typ'18) ^ ", " ^ (exp_toString exp'19) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'20) ^ " . " ^ (exp_toString exp'21)
            ^ ")") ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'22) ^ " . " ^ (exp_toString exp'23)
            ^ ")") ^ ")") ^ ")"))

  fun exp_subst t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (if (Exp.Var.equal (x, y)) then t else (Exp.Var' y))
     | (Exp.Lam (typ'1, (exp'2, exp'3))) =>
         (Exp.Lam'
            ( ((fn _ => (fn _ => (fn t => t))) t x typ'1)
            , (exp'2, (exp_subst t x exp'3))
            ))
     | (Exp.Ap (exp'4, exp'5)) =>
         (Exp.Ap' ((exp_subst t x exp'4), (exp_subst t x exp'5)))
     | Exp.Triv => Exp.Triv'
     | (Exp.Pair (exp'6, exp'7)) =>
         (Exp.Pair' ((exp_subst t x exp'6), (exp_subst t x exp'7)))
     | (Exp.PrL exp'8) => (Exp.PrL' (exp_subst t x exp'8))
     | (Exp.PrR exp'9) => (Exp.PrR' (exp_subst t x exp'9))
     | (Exp.Abort (typ'10, exp'11)) =>
         (Exp.Abort'
            ( ((fn _ => (fn _ => (fn t => t))) t x typ'10)
            , (exp_subst t x exp'11)
            ))
     | (Exp.InL ((typ'12, typ'13), exp'14)) =>
         (Exp.InL'
            ( ( ((fn _ => (fn _ => (fn t => t))) t x typ'12)
              , ((fn _ => (fn _ => (fn t => t))) t x typ'13)
              )
            , (exp_subst t x exp'14)
            ))
     | (Exp.InR ((typ'15, typ'16), exp'17)) =>
         (Exp.InR'
            ( ( ((fn _ => (fn _ => (fn t => t))) t x typ'15)
              , ((fn _ => (fn _ => (fn t => t))) t x typ'16)
              )
            , (exp_subst t x exp'17)
            ))
     | (Exp.Case (typ'18, exp'19, (exp'20, exp'21), (exp'22, exp'23))) =>
         (Exp.Case'
            ( ((fn _ => (fn _ => (fn t => t))) t x typ'18)
            , (exp_subst t x exp'19)
            , (exp'20, (exp_subst t x exp'21))
            , (exp'22, (exp_subst t x exp'23))
            )))

  structure Exp =
  struct
    open Exp
    val toString = exp_toString
    val aequiv = (Abt.aequiv ExpOps.exp_oper_aequiv)
    val subst = exp_subst
  end
end

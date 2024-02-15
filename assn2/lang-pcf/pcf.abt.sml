structure PCF :> PCF =
struct

  structure TypOps =
  struct
    datatype typ = Typ'Nat | Typ'Arrow of typ * typ

    fun typ_bind x i typ =
      (case typ of
         Typ'Nat => Typ'Nat
       | (Typ'Arrow (typ'1, typ'2)) =>
           (Typ'Arrow ((typ_bind x i typ'1), (typ_bind x i typ'2))))

    fun typ_unbind x i typ =
      (case typ of
         Typ'Nat => Typ'Nat
       | (Typ'Arrow (typ'1, typ'2)) =>
           (Typ'Arrow ((typ_unbind x i typ'1), (typ_unbind x i typ'2))))

    fun typ_internal_aequiv (typ1, typ2) =
      (case (typ1, typ2) of
         (Typ'Nat, Typ'Nat) => true
       | ((Typ'Arrow (typ'1, typ'3)), (Typ'Arrow (typ'2, typ'4))) =>
           ((typ_internal_aequiv (typ'1, typ'2))
            andalso (typ_internal_aequiv (typ'3, typ'4)))
       | _ => false)
  end

  datatype typ = Nat | Arrow of typ * typ

  fun typ_view_in vars typ =
    (case typ of
       Nat => (TypOps.Typ'Nat, [])
     | (Arrow (typ'1, typ'2)) =>
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
         end)

  fun typ_view_out vars typ =
    (case typ of
       TypOps.Typ'Nat => (Nat, [])
     | (TypOps.Typ'Arrow (typ'1, typ'2)) =>
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
         end)

  structure Typ = struct datatype typ = datatype typ type t = typ end

  fun typ_aequiv (typ1, typ2) =
    (case (typ1, typ2) of
       (Nat, Nat) => true
     | ((Arrow (typ'1, typ'3)), (Arrow (typ'2, typ'4))) =>
         ((typ_aequiv (typ'1, typ'2)) andalso (typ_aequiv (typ'3, typ'4)))
     | _ => false)

  fun typ_toString typ =
    (case typ of
       Typ.Nat => "Nat"
     | (Typ.Arrow (typ'1, typ'2)) =>
         ("(Arrow "
          ^ ("(" ^ (typ_toString typ'1) ^ ", " ^ (typ_toString typ'2) ^ ")")
          ^ ")"))

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
      Exp'Zero
    | Exp'Succ of exp
    | Exp'Ifz of exp * exp * (string * exp)
    | Exp'Fun of (TypOps.typ * TypOps.typ) * (string * (string * exp))
    | Exp'Ap of exp * exp
    withtype exp = exp_oper Abt.t

    fun exp_oper_bind x i exp =
      (case exp of
         Exp'Zero => Exp'Zero
       | (Exp'Succ exp'1) => (Exp'Succ (Abt.bind exp_oper_bind x i exp'1))
       | (Exp'Ifz (exp'2, exp'3, (exp'4, exp'5))) =>
           (Exp'Ifz
              ( (Abt.bind exp_oper_bind x i exp'2)
              , (Abt.bind exp_oper_bind x i exp'3)
              , (exp'4, (Abt.bind exp_oper_bind x i exp'5))
              ))
       | (Exp'Fun ((typ'6, typ'7), (exp'8, (exp'9, exp'10)))) =>
           (Exp'Fun
              ( ((TypOps.typ_bind x i typ'6), (TypOps.typ_bind x i typ'7))
              , (exp'8, (exp'9, (Abt.bind exp_oper_bind x i exp'10)))
              ))
       | (Exp'Ap (exp'11, exp'12)) =>
           (Exp'Ap
              ( (Abt.bind exp_oper_bind x i exp'11)
              , (Abt.bind exp_oper_bind x i exp'12)
              )))

    fun exp_oper_unbind x i exp =
      (case exp of
         Exp'Zero => Exp'Zero
       | (Exp'Succ exp'1) => (Exp'Succ (Abt.unbind exp_oper_unbind x i exp'1))
       | (Exp'Ifz (exp'2, exp'3, (exp'4, exp'5))) =>
           (Exp'Ifz
              ( (Abt.unbind exp_oper_unbind x i exp'2)
              , (Abt.unbind exp_oper_unbind x i exp'3)
              , (exp'4, (Abt.unbind exp_oper_unbind x i exp'5))
              ))
       | (Exp'Fun ((typ'6, typ'7), (exp'8, (exp'9, exp'10)))) =>
           (Exp'Fun
              ( ((TypOps.typ_unbind x i typ'6), (TypOps.typ_unbind x i typ'7))
              , (exp'8, (exp'9, (Abt.unbind exp_oper_unbind x i exp'10)))
              ))
       | (Exp'Ap (exp'11, exp'12)) =>
           (Exp'Ap
              ( (Abt.unbind exp_oper_unbind x i exp'11)
              , (Abt.unbind exp_oper_unbind x i exp'12)
              )))

    fun exp_oper_aequiv (exp1, exp2) =
      (case (exp1, exp2) of
         (Exp'Zero, Exp'Zero) => true
       | ((Exp'Succ exp'1), (Exp'Succ exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ( (Exp'Ifz (exp'1, exp'3, (_, exp'5)))
         , (Exp'Ifz (exp'2, exp'4, (_, exp'6)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ( (Exp'Fun ((typ'1, typ'3), (_, (_, exp'5))))
         , (Exp'Fun ((typ'2, typ'4), (_, (_, exp'6))))
         ) =>
           (((Typ.internal_aequiv (typ'1, typ'2))
             andalso (Typ.internal_aequiv (typ'3, typ'4)))
            andalso
            (true
             andalso
             (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))))
       | ((Exp'Ap (exp'1, exp'3)), (Exp'Ap (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
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
    | Zero
    | Succ of exp
    | Ifz of exp * exp * (expVar * exp)
    | Fun of (Typ.t * Typ.t) * (expVar * (expVar * exp))
    | Ap of exp * exp

    fun view_in vars exp =
      (case exp of
         (Var x) => (Abt.Var x)
       | Zero => (Abt.Oper ExpOps.Exp'Zero)
       | (Succ exp'1) =>
           (Abt.Oper (ExpOps.Exp'Succ ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'1 vars)
              , []
              ))))
       | (Ifz (exp'2, exp'3, (exp'4, exp'5))) =>
           (Abt.Oper (ExpOps.Exp'Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'3 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = exp'4
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'5 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Fun ((typ'6, typ'7), (exp'8, (exp'9, exp'10)))) =>
           (Abt.Oper (ExpOps.Exp'Fun ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) = (typ_view_in vars typ'6)
                    val (t1, vars'1) = (typ_view_in vars typ'7)
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'8
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      let
                        val (t, vars') = let val var = exp'9
                                         in ((Temp.toUserString var), [var])
                                         end
                        val vars = (vars' @ vars)
                        val (t', vars') =
                          ( (List.foldl
                               (fn (x, acc) =>
                                  (Abt.into ExpOps.exp_oper_bind
                                     (Abt.Binding (x, acc)))) exp'10 vars)
                          , []
                          )
                      in
                        ((t, t'), vars')
                      end
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Ap (exp'11, exp'12)) =>
           (Abt.Oper (ExpOps.Exp'Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'11 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'12 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))))

    fun oper_view_out vars exp =
      (case exp of
         ExpOps.Exp'Zero => Zero
       | (ExpOps.Exp'Succ exp'1) =>
           (Succ ((fn (x, _) => x)
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
              )))
       | (ExpOps.Exp'Ifz (exp'2, exp'3, (exp'4, exp'5))) =>
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
                          end) exp'2 vars)
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
                          end) exp'3 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'4)
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
                              end) exp'5 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ExpOps.Exp'Fun ((typ'6, typ'7), (exp'8, (exp'9, exp'10)))) =>
           (Fun ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) = (typ_view_out vars typ'6)
                    val (t1, vars'1) = (typ_view_out vars typ'7)
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'8)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      let
                        val (t, vars') = let val x = (Temp.new exp'9)
                                         in (x, [x])
                                         end
                        val vars = (vars' @ vars)
                        val (t', vars') =
                          ( (List.foldr
                               (fn (x, acc) =>
                                  let
                                    val (Abt.Binding (_, acc')) =
                                      (Abt.out ExpOps.exp_oper_unbind
                                         (Abt.unbind ExpOps.exp_oper_unbind x ~1
                                            acc))
                                  in
                                    acc'
                                  end) exp'10 vars)
                          , []
                          )
                      in
                        ((t, t'), vars')
                      end
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Ap (exp'11, exp'12)) =>
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
                          end) exp'11 vars)
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
                          end) exp'12 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
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
    val Zero' = (into Zero)
    val Succ' = (into o Succ)
    val Ifz' = (into o Ifz)
    val Fun' = (into o Fun)
    val Ap' = (into o Ap)
  end

  fun exp_toString exp =
    (case (Exp.out exp) of
       (Exp.Var x) => (Exp.Var.toString x)
     | Exp.Zero => "Zero"
     | (Exp.Succ exp'1) => ("(Succ " ^ (exp_toString exp'1) ^ ")")
     | (Exp.Ifz (exp'2, exp'3, (exp'4, exp'5))) =>
         ("(Ifz "
          ^
          ("(" ^ (exp_toString exp'2) ^ ", " ^ (exp_toString exp'3) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'4) ^ " . " ^ (exp_toString exp'5) ^ ")")
           ^ ")") ^ ")")
     | (Exp.Fun ((typ'6, typ'7), (exp'8, (exp'9, exp'10)))) =>
         ("(Fun "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'6) ^ ", " ^ (Typ.toString typ'7) ^ ")")
           ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'8) ^ " . "
            ^
            ("(" ^ (Exp.Var.toString exp'9) ^ " . " ^ (exp_toString exp'10)
             ^ ")") ^ ")") ^ ")") ^ ")")
     | (Exp.Ap (exp'11, exp'12)) =>
         ("(Ap "
          ^ ("(" ^ (exp_toString exp'11) ^ ", " ^ (exp_toString exp'12) ^ ")")
          ^ ")"))

  fun exp_subst t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (if (Exp.Var.equal (x, y)) then t else (Exp.Var' y))
     | Exp.Zero => Exp.Zero'
     | (Exp.Succ exp'1) => (Exp.Succ' (exp_subst t x exp'1))
     | (Exp.Ifz (exp'2, exp'3, (exp'4, exp'5))) =>
         (Exp.Ifz'
            ( (exp_subst t x exp'2)
            , (exp_subst t x exp'3)
            , (exp'4, (exp_subst t x exp'5))
            ))
     | (Exp.Fun ((typ'6, typ'7), (exp'8, (exp'9, exp'10)))) =>
         (Exp.Fun'
            ( ( ((fn _ => (fn _ => (fn t => t))) t x typ'6)
              , ((fn _ => (fn _ => (fn t => t))) t x typ'7)
              )
            , (exp'8, (exp'9, (exp_subst t x exp'10)))
            ))
     | (Exp.Ap (exp'11, exp'12)) =>
         (Exp.Ap' ((exp_subst t x exp'11), (exp_subst t x exp'12))))

  structure Exp =
  struct
    open Exp
    val toString = exp_toString
    val aequiv = (Abt.aequiv ExpOps.exp_oper_aequiv)
    val subst = exp_subst
  end
end

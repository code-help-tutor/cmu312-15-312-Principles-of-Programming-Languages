functor CA
  (structure String:
   sig
     type t
     val equal: t * t -> bool
     val toString: t -> string
   end
   structure Oper:
   sig
     type t
     val equal: t * t -> bool
     val toString: t -> string
   end
   structure Int:
   sig
     type t
     val equal: t * t -> bool
     val toString: t -> string
   end
   structure Bool:
   sig
     type t
     val equal: t * t -> bool
     val toString: t -> string
   end) :> CA
           where type String.t = String.t
           where type Oper.t = Oper.t
           where type Int.t = Int.t
           where type Bool.t = Bool.t =
struct
  structure Chan = Temp

  structure String = String

  structure Oper = Oper

  structure Int = Int

  structure Bool = Bool

  structure TypOps =
  struct
    datatype typ_oper =
      Typ'Nat
    | Typ'Bool
    | Typ'Unit
    | Typ'Void
    | Typ'String
    | Typ'Arr of typ * typ
    | Typ'Star of typ * typ
    | Typ'Sum of typ * typ
    | Typ'Rec of string * typ
    | Typ'Cmd of typ
    | Typ'Chan of typ
    withtype typ = typ_oper Abt.t

    fun typ_oper_bind x i typ =
      (case typ of
         Typ'Nat => Typ'Nat
       | Typ'Bool => Typ'Bool
       | Typ'Unit => Typ'Unit
       | Typ'Void => Typ'Void
       | Typ'String => Typ'String
       | (Typ'Arr (typ'1, typ'2)) =>
           (Typ'Arr
              ( (Abt.bind typ_oper_bind x i typ'1)
              , (Abt.bind typ_oper_bind x i typ'2)
              ))
       | (Typ'Star (typ'3, typ'4)) =>
           (Typ'Star
              ( (Abt.bind typ_oper_bind x i typ'3)
              , (Abt.bind typ_oper_bind x i typ'4)
              ))
       | (Typ'Sum (typ'5, typ'6)) =>
           (Typ'Sum
              ( (Abt.bind typ_oper_bind x i typ'5)
              , (Abt.bind typ_oper_bind x i typ'6)
              ))
       | (Typ'Rec (typ'7, typ'8)) =>
           (Typ'Rec (typ'7, (Abt.bind typ_oper_bind x i typ'8)))
       | (Typ'Cmd typ'9) => (Typ'Cmd (Abt.bind typ_oper_bind x i typ'9))
       | (Typ'Chan typ'10) => (Typ'Chan (Abt.bind typ_oper_bind x i typ'10)))

    fun typ_oper_unbind x i typ =
      (case typ of
         Typ'Nat => Typ'Nat
       | Typ'Bool => Typ'Bool
       | Typ'Unit => Typ'Unit
       | Typ'Void => Typ'Void
       | Typ'String => Typ'String
       | (Typ'Arr (typ'1, typ'2)) =>
           (Typ'Arr
              ( (Abt.unbind typ_oper_unbind x i typ'1)
              , (Abt.unbind typ_oper_unbind x i typ'2)
              ))
       | (Typ'Star (typ'3, typ'4)) =>
           (Typ'Star
              ( (Abt.unbind typ_oper_unbind x i typ'3)
              , (Abt.unbind typ_oper_unbind x i typ'4)
              ))
       | (Typ'Sum (typ'5, typ'6)) =>
           (Typ'Sum
              ( (Abt.unbind typ_oper_unbind x i typ'5)
              , (Abt.unbind typ_oper_unbind x i typ'6)
              ))
       | (Typ'Rec (typ'7, typ'8)) =>
           (Typ'Rec (typ'7, (Abt.unbind typ_oper_unbind x i typ'8)))
       | (Typ'Cmd typ'9) => (Typ'Cmd (Abt.unbind typ_oper_unbind x i typ'9))
       | (Typ'Chan typ'10) => (Typ'Chan (Abt.unbind typ_oper_unbind x i typ'10)))

    fun typ_oper_aequiv (typ1, typ2) =
      (case (typ1, typ2) of
         (Typ'Nat, Typ'Nat) => true
       | (Typ'Bool, Typ'Bool) => true
       | (Typ'Unit, Typ'Unit) => true
       | (Typ'Void, Typ'Void) => true
       | (Typ'String, Typ'String) => true
       | ((Typ'Arr (typ'1, typ'3)), (Typ'Arr (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | ((Typ'Star (typ'1, typ'3)), (Typ'Star (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | ((Typ'Sum (typ'1, typ'3)), (Typ'Sum (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | ((Typ'Rec (_, typ'1)), (Typ'Rec (_, typ'2))) =>
           (true andalso ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2)))
       | ((Typ'Cmd typ'1), (Typ'Cmd typ'2)) =>
           ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
       | ((Typ'Chan typ'1), (Typ'Chan typ'2)) =>
           ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
       | _ => false)
  end

  structure Typ =
  struct
    type typVar = Temp.t
    type typ = TypOps.typ
    type t = typ

    structure Var = Temp

    datatype view =
      Var of typVar
    | Nat
    | Bool
    | Unit
    | Void
    | String
    | Arr of typ * typ
    | Star of typ * typ
    | Sum of typ * typ
    | Rec of typVar * typ
    | Cmd of typ
    | Chan of typ

    fun view_in vars typ =
      (case typ of
         (Var x) => (Abt.Var x)
       | Nat => (Abt.Oper TypOps.Typ'Nat)
       | Bool => (Abt.Oper TypOps.Typ'Bool)
       | Unit => (Abt.Oper TypOps.Typ'Unit)
       | Void => (Abt.Oper TypOps.Typ'Void)
       | String => (Abt.Oper TypOps.Typ'String)
       | (Arr (typ'1, typ'2)) =>
           (Abt.Oper (TypOps.Typ'Arr ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'1 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'2 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Star (typ'3, typ'4)) =>
           (Abt.Oper (TypOps.Typ'Star ((fn (x, _) => x)
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
       | (Rec (typ'7, typ'8)) =>
           (Abt.Oper (TypOps.Typ'Rec ((fn (x, _) => x)
              let
                val (t, vars') = let val var = typ'7
                                 in ((Temp.toUserString var), [var])
                                 end
                val vars = (vars' @ vars)
                val (t', vars') =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'8 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end)))
       | (Cmd typ'9) =>
           (Abt.Oper (TypOps.Typ'Cmd ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'9 vars)
              , []
              ))))
       | (Chan typ'10) =>
           (Abt.Oper (TypOps.Typ'Chan ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'10 vars)
              , []
              )))))

    fun oper_view_out vars typ =
      (case typ of
         TypOps.Typ'Nat => Nat
       | TypOps.Typ'Bool => Bool
       | TypOps.Typ'Unit => Unit
       | TypOps.Typ'Void => Void
       | TypOps.Typ'String => String
       | (TypOps.Typ'Arr (typ'1, typ'2)) =>
           (Arr ((fn (x, _) => x)
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
                          end) typ'1 vars)
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
                          end) typ'2 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (TypOps.Typ'Star (typ'3, typ'4)) =>
           (Star ((fn (x, _) => x)
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
       | (TypOps.Typ'Rec (typ'7, typ'8)) =>
           (Rec ((fn (x, _) => x)
              let
                val (t, vars') = let val x = (Temp.new typ'7) in (x, [x]) end
                val vars = (vars' @ vars)
                val (t', vars') =
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
                ((t, t'), vars')
              end))
       | (TypOps.Typ'Cmd typ'9) =>
           (Cmd ((fn (x, _) => x)
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
              )))
       | (TypOps.Typ'Chan typ'10) =>
           (Chan ((fn (x, _) => x)
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
              ))))

    fun view_out t =
      (case t of
         (Abt.Var x) => (Var x)
       | (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into TypOps.typ_oper_bind (view_in [] v))
    fun out typ =
      (view_out (Abt.out TypOps.typ_oper_unbind typ))

    val Var' = (into o Var)
    val Nat' = (into Nat)
    val Bool' = (into Bool)
    val Unit' = (into Unit)
    val Void' = (into Void)
    val String' = (into String)
    val Arr' = (into o Arr)
    val Star' = (into o Star)
    val Sum' = (into o Sum)
    val Rec' = (into o Rec)
    val Cmd' = (into o Cmd)
    val Chan' = (into o Chan)
  end

  fun typ_toString typ =
    (case (Typ.out typ) of
       (Typ.Var x) => (Typ.Var.toString x)
     | Typ.Nat => "Nat"
     | Typ.Bool => "Bool"
     | Typ.Unit => "Unit"
     | Typ.Void => "Void"
     | Typ.String => "String"
     | (Typ.Arr (typ'1, typ'2)) =>
         ("(Arr "
          ^ ("(" ^ (typ_toString typ'1) ^ ", " ^ (typ_toString typ'2) ^ ")")
          ^ ")")
     | (Typ.Star (typ'3, typ'4)) =>
         ("(Star "
          ^ ("(" ^ (typ_toString typ'3) ^ ", " ^ (typ_toString typ'4) ^ ")")
          ^ ")")
     | (Typ.Sum (typ'5, typ'6)) =>
         ("(Sum "
          ^ ("(" ^ (typ_toString typ'5) ^ ", " ^ (typ_toString typ'6) ^ ")")
          ^ ")")
     | (Typ.Rec (typ'7, typ'8)) =>
         ("(Rec "
          ^
          ("(" ^ (Typ.Var.toString typ'7) ^ " . " ^ (typ_toString typ'8) ^ ")")
          ^ ")")
     | (Typ.Cmd typ'9) => ("(Cmd " ^ (typ_toString typ'9) ^ ")")
     | (Typ.Chan typ'10) => ("(Chan " ^ (typ_toString typ'10) ^ ")"))

  fun typ_subst t x typ =
    (case (Typ.out typ) of
       (Typ.Var y) => (if (Typ.Var.equal (x, y)) then t else (Typ.Var' y))
     | Typ.Nat => Typ.Nat'
     | Typ.Bool => Typ.Bool'
     | Typ.Unit => Typ.Unit'
     | Typ.Void => Typ.Void'
     | Typ.String => Typ.String'
     | (Typ.Arr (typ'1, typ'2)) =>
         (Typ.Arr' ((typ_subst t x typ'1), (typ_subst t x typ'2)))
     | (Typ.Star (typ'3, typ'4)) =>
         (Typ.Star' ((typ_subst t x typ'3), (typ_subst t x typ'4)))
     | (Typ.Sum (typ'5, typ'6)) =>
         (Typ.Sum' ((typ_subst t x typ'5), (typ_subst t x typ'6)))
     | (Typ.Rec (typ'7, typ'8)) => (Typ.Rec' (typ'7, (typ_subst t x typ'8)))
     | (Typ.Cmd typ'9) => (Typ.Cmd' (typ_subst t x typ'9))
     | (Typ.Chan typ'10) => (Typ.Chan' (typ_subst t x typ'10)))

  structure Typ =
  struct
    open Typ
    val toString = typ_toString
    val aequiv = (Abt.aequiv TypOps.typ_oper_aequiv)
    val subst = typ_subst
  end

  structure ExpCmdOps =
  struct
    datatype exp_oper =
      Exp'String of String.t
    | Exp'Num of Int.t
    | Exp'Succ of exp
    | Exp'Ifz of exp * exp * (string * exp)
    | Exp'Bool of Bool.t
    | Exp'Negate of exp
    | Exp'If of exp * (exp * exp)
    | Exp'Binop of Oper.t * exp * exp
    | Exp'Fun of (Typ.t * Typ.t) * ((string * string) * exp)
    | Exp'Lam of Typ.t * (string * exp)
    | Exp'App of exp * exp
    | Exp'Let of exp * (string * exp)
    | Exp'Triv
    | Exp'Pair of exp * exp
    | Exp'Split of exp * ((string * string) * exp)
    | Exp'Inl of Typ.t * exp
    | Exp'Inr of Typ.t * exp
    | Exp'Case of exp * (string * exp) * (string * exp)
    | Exp'Abort of Typ.t * exp
    | Exp'Fold of Typ.t * exp
    | Exp'Unfold of exp
    | Exp'Cmd of cmd
    | Exp'ChnRef of unit Abt.t
    and cmd_oper =
      Cmd'Ret of exp
    | Cmd'Bind of exp * (string * cmd)
    | Cmd'Spawn of exp
    | Cmd'Emit of exp * exp
    | Cmd'Sync of exp
    | Cmd'NewChn of Typ.t * (string * cmd)
    | Cmd'Print of exp
    withtype exp = exp_oper Abt.t
    and cmd = cmd_oper Abt.t

    fun exp_oper_bind x i exp =
      (case exp of
         (Exp'String string'1) => (Exp'String string'1)
       | (Exp'Num int'2) => (Exp'Num int'2)
       | (Exp'Succ exp'3) => (Exp'Succ (Abt.bind exp_oper_bind x i exp'3))
       | (Exp'Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
           (Exp'Ifz
              ( (Abt.bind exp_oper_bind x i exp'4)
              , (Abt.bind exp_oper_bind x i exp'5)
              , (exp'6, (Abt.bind exp_oper_bind x i exp'7))
              ))
       | (Exp'Bool bool'8) => (Exp'Bool bool'8)
       | (Exp'Negate exp'9) => (Exp'Negate (Abt.bind exp_oper_bind x i exp'9))
       | (Exp'If (exp'10, (exp'11, exp'12))) =>
           (Exp'If
              ( (Abt.bind exp_oper_bind x i exp'10)
              , ( (Abt.bind exp_oper_bind x i exp'11)
                , (Abt.bind exp_oper_bind x i exp'12)
                )
              ))
       | (Exp'Binop (oper'13, exp'14, exp'15)) =>
           (Exp'Binop
              ( oper'13
              , (Abt.bind exp_oper_bind x i exp'14)
              , (Abt.bind exp_oper_bind x i exp'15)
              ))
       | (Exp'Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
           (Exp'Fun
              ( ( (Abt.bind TypOps.typ_oper_bind x i typ'16)
                , (Abt.bind TypOps.typ_oper_bind x i typ'17)
                )
              , ((exp'18, exp'19), (Abt.bind exp_oper_bind x i exp'20))
              ))
       | (Exp'Lam (typ'21, (exp'22, exp'23))) =>
           (Exp'Lam
              ( (Abt.bind TypOps.typ_oper_bind x i typ'21)
              , (exp'22, (Abt.bind exp_oper_bind x i exp'23))
              ))
       | (Exp'App (exp'24, exp'25)) =>
           (Exp'App
              ( (Abt.bind exp_oper_bind x i exp'24)
              , (Abt.bind exp_oper_bind x i exp'25)
              ))
       | (Exp'Let (exp'26, (exp'27, exp'28))) =>
           (Exp'Let
              ( (Abt.bind exp_oper_bind x i exp'26)
              , (exp'27, (Abt.bind exp_oper_bind x i exp'28))
              ))
       | Exp'Triv => Exp'Triv
       | (Exp'Pair (exp'29, exp'30)) =>
           (Exp'Pair
              ( (Abt.bind exp_oper_bind x i exp'29)
              , (Abt.bind exp_oper_bind x i exp'30)
              ))
       | (Exp'Split (exp'31, ((exp'32, exp'33), exp'34))) =>
           (Exp'Split
              ( (Abt.bind exp_oper_bind x i exp'31)
              , ((exp'32, exp'33), (Abt.bind exp_oper_bind x i exp'34))
              ))
       | (Exp'Inl (typ'35, exp'36)) =>
           (Exp'Inl
              ( (Abt.bind TypOps.typ_oper_bind x i typ'35)
              , (Abt.bind exp_oper_bind x i exp'36)
              ))
       | (Exp'Inr (typ'37, exp'38)) =>
           (Exp'Inr
              ( (Abt.bind TypOps.typ_oper_bind x i typ'37)
              , (Abt.bind exp_oper_bind x i exp'38)
              ))
       | (Exp'Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
           (Exp'Case
              ( (Abt.bind exp_oper_bind x i exp'39)
              , (exp'40, (Abt.bind exp_oper_bind x i exp'41))
              , (exp'42, (Abt.bind exp_oper_bind x i exp'43))
              ))
       | (Exp'Abort (typ'44, exp'45)) =>
           (Exp'Abort
              ( (Abt.bind TypOps.typ_oper_bind x i typ'44)
              , (Abt.bind exp_oper_bind x i exp'45)
              ))
       | (Exp'Fold (typ'46, exp'47)) =>
           (Exp'Fold
              ( (Abt.bind TypOps.typ_oper_bind x i typ'46)
              , (Abt.bind exp_oper_bind x i exp'47)
              ))
       | (Exp'Unfold exp'48) => (Exp'Unfold (Abt.bind exp_oper_bind x i exp'48))
       | (Exp'Cmd cmd'49) => (Exp'Cmd (Abt.bind cmd_oper_bind x i cmd'49))
       | (Exp'ChnRef chan'50) =>
           (Exp'ChnRef (Abt.bind (fn _ => (fn _ => (fn t => t))) x i chan'50)))
    and cmd_oper_bind x i cmd =
      (case cmd of
         (Cmd'Ret exp'1) => (Cmd'Ret (Abt.bind exp_oper_bind x i exp'1))
       | (Cmd'Bind (exp'2, (exp'3, cmd'4))) =>
           (Cmd'Bind
              ( (Abt.bind exp_oper_bind x i exp'2)
              , (exp'3, (Abt.bind cmd_oper_bind x i cmd'4))
              ))
       | (Cmd'Spawn exp'5) => (Cmd'Spawn (Abt.bind exp_oper_bind x i exp'5))
       | (Cmd'Emit (exp'6, exp'7)) =>
           (Cmd'Emit
              ( (Abt.bind exp_oper_bind x i exp'6)
              , (Abt.bind exp_oper_bind x i exp'7)
              ))
       | (Cmd'Sync exp'8) => (Cmd'Sync (Abt.bind exp_oper_bind x i exp'8))
       | (Cmd'NewChn (typ'9, (chan'10, cmd'11))) =>
           (Cmd'NewChn
              ( (Abt.bind TypOps.typ_oper_bind x i typ'9)
              , (chan'10, (Abt.bind cmd_oper_bind x i cmd'11))
              ))
       | (Cmd'Print exp'12) => (Cmd'Print (Abt.bind exp_oper_bind x i exp'12)))

    fun exp_oper_unbind x i exp =
      (case exp of
         (Exp'String string'1) => (Exp'String string'1)
       | (Exp'Num int'2) => (Exp'Num int'2)
       | (Exp'Succ exp'3) => (Exp'Succ (Abt.unbind exp_oper_unbind x i exp'3))
       | (Exp'Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
           (Exp'Ifz
              ( (Abt.unbind exp_oper_unbind x i exp'4)
              , (Abt.unbind exp_oper_unbind x i exp'5)
              , (exp'6, (Abt.unbind exp_oper_unbind x i exp'7))
              ))
       | (Exp'Bool bool'8) => (Exp'Bool bool'8)
       | (Exp'Negate exp'9) =>
           (Exp'Negate (Abt.unbind exp_oper_unbind x i exp'9))
       | (Exp'If (exp'10, (exp'11, exp'12))) =>
           (Exp'If
              ( (Abt.unbind exp_oper_unbind x i exp'10)
              , ( (Abt.unbind exp_oper_unbind x i exp'11)
                , (Abt.unbind exp_oper_unbind x i exp'12)
                )
              ))
       | (Exp'Binop (oper'13, exp'14, exp'15)) =>
           (Exp'Binop
              ( oper'13
              , (Abt.unbind exp_oper_unbind x i exp'14)
              , (Abt.unbind exp_oper_unbind x i exp'15)
              ))
       | (Exp'Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
           (Exp'Fun
              ( ( (Abt.unbind TypOps.typ_oper_unbind x i typ'16)
                , (Abt.unbind TypOps.typ_oper_unbind x i typ'17)
                )
              , ((exp'18, exp'19), (Abt.unbind exp_oper_unbind x i exp'20))
              ))
       | (Exp'Lam (typ'21, (exp'22, exp'23))) =>
           (Exp'Lam
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'21)
              , (exp'22, (Abt.unbind exp_oper_unbind x i exp'23))
              ))
       | (Exp'App (exp'24, exp'25)) =>
           (Exp'App
              ( (Abt.unbind exp_oper_unbind x i exp'24)
              , (Abt.unbind exp_oper_unbind x i exp'25)
              ))
       | (Exp'Let (exp'26, (exp'27, exp'28))) =>
           (Exp'Let
              ( (Abt.unbind exp_oper_unbind x i exp'26)
              , (exp'27, (Abt.unbind exp_oper_unbind x i exp'28))
              ))
       | Exp'Triv => Exp'Triv
       | (Exp'Pair (exp'29, exp'30)) =>
           (Exp'Pair
              ( (Abt.unbind exp_oper_unbind x i exp'29)
              , (Abt.unbind exp_oper_unbind x i exp'30)
              ))
       | (Exp'Split (exp'31, ((exp'32, exp'33), exp'34))) =>
           (Exp'Split
              ( (Abt.unbind exp_oper_unbind x i exp'31)
              , ((exp'32, exp'33), (Abt.unbind exp_oper_unbind x i exp'34))
              ))
       | (Exp'Inl (typ'35, exp'36)) =>
           (Exp'Inl
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'35)
              , (Abt.unbind exp_oper_unbind x i exp'36)
              ))
       | (Exp'Inr (typ'37, exp'38)) =>
           (Exp'Inr
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'37)
              , (Abt.unbind exp_oper_unbind x i exp'38)
              ))
       | (Exp'Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
           (Exp'Case
              ( (Abt.unbind exp_oper_unbind x i exp'39)
              , (exp'40, (Abt.unbind exp_oper_unbind x i exp'41))
              , (exp'42, (Abt.unbind exp_oper_unbind x i exp'43))
              ))
       | (Exp'Abort (typ'44, exp'45)) =>
           (Exp'Abort
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'44)
              , (Abt.unbind exp_oper_unbind x i exp'45)
              ))
       | (Exp'Fold (typ'46, exp'47)) =>
           (Exp'Fold
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'46)
              , (Abt.unbind exp_oper_unbind x i exp'47)
              ))
       | (Exp'Unfold exp'48) =>
           (Exp'Unfold (Abt.unbind exp_oper_unbind x i exp'48))
       | (Exp'Cmd cmd'49) => (Exp'Cmd (Abt.unbind cmd_oper_unbind x i cmd'49))
       | (Exp'ChnRef chan'50) =>
           (Exp'ChnRef (Abt.unbind (fn _ => (fn _ => (fn t => t))) x i chan'50)))
    and cmd_oper_unbind x i cmd =
      (case cmd of
         (Cmd'Ret exp'1) => (Cmd'Ret (Abt.unbind exp_oper_unbind x i exp'1))
       | (Cmd'Bind (exp'2, (exp'3, cmd'4))) =>
           (Cmd'Bind
              ( (Abt.unbind exp_oper_unbind x i exp'2)
              , (exp'3, (Abt.unbind cmd_oper_unbind x i cmd'4))
              ))
       | (Cmd'Spawn exp'5) => (Cmd'Spawn (Abt.unbind exp_oper_unbind x i exp'5))
       | (Cmd'Emit (exp'6, exp'7)) =>
           (Cmd'Emit
              ( (Abt.unbind exp_oper_unbind x i exp'6)
              , (Abt.unbind exp_oper_unbind x i exp'7)
              ))
       | (Cmd'Sync exp'8) => (Cmd'Sync (Abt.unbind exp_oper_unbind x i exp'8))
       | (Cmd'NewChn (typ'9, (chan'10, cmd'11))) =>
           (Cmd'NewChn
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'9)
              , (chan'10, (Abt.unbind cmd_oper_unbind x i cmd'11))
              ))
       | (Cmd'Print exp'12) =>
           (Cmd'Print (Abt.unbind exp_oper_unbind x i exp'12)))

    fun exp_oper_aequiv (exp1, exp2) =
      (case (exp1, exp2) of
         ((Exp'String string'1), (Exp'String string'2)) =>
           (String.equal (string'1, string'2))
       | ((Exp'Num int'1), (Exp'Num int'2)) => (Int.equal (int'1, int'2))
       | ((Exp'Succ exp'1), (Exp'Succ exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ( (Exp'Ifz (exp'1, exp'3, (_, exp'5)))
         , (Exp'Ifz (exp'2, exp'4, (_, exp'6)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ((Exp'Bool bool'1), (Exp'Bool bool'2)) => (Bool.equal (bool'1, bool'2))
       | ((Exp'Negate exp'1), (Exp'Negate exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Exp'If (exp'1, (exp'3, exp'5))), (Exp'If (exp'2, (exp'4, exp'6)))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso
            (((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ( (Exp'Binop (oper'1, exp'3, exp'5))
         , (Exp'Binop (oper'2, exp'4, exp'6))
         ) =>
           ((Oper.equal (oper'1, oper'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6)))
       | ( (Exp'Fun ((typ'1, typ'3), ((_, _), exp'5)))
         , (Exp'Fun ((typ'2, typ'4), ((_, _), exp'6)))
         ) =>
           (((Typ.aequiv (typ'1, typ'2)) andalso (Typ.aequiv (typ'3, typ'4)))
            andalso
            ((true andalso true)
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ((Exp'Lam (typ'1, (_, exp'3))), (Exp'Lam (typ'2, (_, exp'4)))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'App (exp'1, exp'3)), (Exp'App (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Let (exp'1, (_, exp'3))), (Exp'Let (exp'2, (_, exp'4)))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | (Exp'Triv, Exp'Triv) => true
       | ((Exp'Pair (exp'1, exp'3)), (Exp'Pair (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ( (Exp'Split (exp'1, ((_, _), exp'3)))
         , (Exp'Split (exp'2, ((_, _), exp'4)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso
            ((true andalso true)
             andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))))
       | ((Exp'Inl (typ'1, exp'3)), (Exp'Inl (typ'2, exp'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Inr (typ'1, exp'3)), (Exp'Inr (typ'2, exp'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ( (Exp'Case (exp'1, (_, exp'3), (_, exp'5)))
         , (Exp'Case (exp'2, (_, exp'4), (_, exp'6)))
         ) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
            andalso (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
       | ((Exp'Abort (typ'1, exp'3)), (Exp'Abort (typ'2, exp'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Fold (typ'1, exp'3)), (Exp'Fold (typ'2, exp'4))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Unfold exp'1), (Exp'Unfold exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Exp'Cmd cmd'1), (Exp'Cmd cmd'2)) =>
           ((Abt.aequiv cmd_oper_aequiv) (cmd'1, cmd'2))
       | ((Exp'ChnRef chan'1), (Exp'ChnRef chan'2)) =>
           (Abt.aequiv (fn _ => true) (chan'1, chan'2))
       | _ => false)
    and cmd_oper_aequiv (cmd1, cmd2) =
      (case (cmd1, cmd2) of
         ((Cmd'Ret exp'1), (Cmd'Ret exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Cmd'Bind (exp'1, (_, cmd'3))), (Cmd'Bind (exp'2, (_, cmd'4)))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso (true andalso ((Abt.aequiv cmd_oper_aequiv) (cmd'3, cmd'4))))
       | ((Cmd'Spawn exp'1), (Cmd'Spawn exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Cmd'Emit (exp'1, exp'3)), (Cmd'Emit (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Cmd'Sync exp'1), (Cmd'Sync exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Cmd'NewChn (typ'1, (_, cmd'3))), (Cmd'NewChn (typ'2, (_, cmd'4)))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso (true andalso ((Abt.aequiv cmd_oper_aequiv) (cmd'3, cmd'4))))
       | ((Cmd'Print exp'1), (Cmd'Print exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | _ => false)
  end

  structure Exp =
  struct
    type cmd = ExpCmdOps.cmd
    type expVar = Temp.t
    type exp = ExpCmdOps.exp
    type t = exp

    structure Var = Temp

    datatype view =
      Var of expVar
    | String of String.t
    | Num of Int.t
    | Succ of exp
    | Ifz of exp * exp * (expVar * exp)
    | Bool of Bool.t
    | Negate of exp
    | If of exp * (exp * exp)
    | Binop of Oper.t * exp * exp
    | Fun of (Typ.t * Typ.t) * ((expVar * expVar) * exp)
    | Lam of Typ.t * (expVar * exp)
    | App of exp * exp
    | Let of exp * (expVar * exp)
    | Triv
    | Pair of exp * exp
    | Split of exp * ((expVar * expVar) * exp)
    | Inl of Typ.t * exp
    | Inr of Typ.t * exp
    | Case of exp * (expVar * exp) * (expVar * exp)
    | Abort of Typ.t * exp
    | Fold of Typ.t * exp
    | Unfold of exp
    | Cmd of cmd
    | ChnRef of Chan.t

    fun view_in vars exp =
      (case exp of
         (Var x) => (Abt.Var x)
       | (String string'1) =>
           (Abt.Oper (ExpCmdOps.Exp'String ((fn (x, _) => x) (string'1, []))))
       | (Num int'2) =>
           (Abt.Oper (ExpCmdOps.Exp'Num ((fn (x, _) => x) (int'2, []))))
       | (Succ exp'3) =>
           (Abt.Oper (ExpCmdOps.Exp'Succ ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'3 vars)
              , []
              ))))
       | (Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
           (Abt.Oper (ExpCmdOps.Exp'Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'4 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'5 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = exp'6
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'7 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Bool bool'8) =>
           (Abt.Oper (ExpCmdOps.Exp'Bool ((fn (x, _) => x) (bool'8, []))))
       | (Negate exp'9) =>
           (Abt.Oper (ExpCmdOps.Exp'Negate ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'9 vars)
              , []
              ))))
       | (If (exp'10, (exp'11, exp'12))) =>
           (Abt.Oper (ExpCmdOps.Exp'If ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'10 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'11 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'12 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Binop (oper'13, exp'14, exp'15)) =>
           (Abt.Oper (ExpCmdOps.Exp'Binop ((fn (x, _) => x)
              let
                val (t0, vars'0) = (oper'13, [])
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'14 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'15 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
           (Abt.Oper (ExpCmdOps.Exp'Fun ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'16 vars)
                      , []
                      )
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'17 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val var = exp'18
                                           in ((Temp.toUserString var), [var])
                                           end
                        val (t1, vars'1) = let val var = exp'19
                                           in ((Temp.toUserString var), [var])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'20 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Lam (typ'21, (exp'22, exp'23))) =>
           (Abt.Oper (ExpCmdOps.Exp'Lam ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'21 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'22
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'23 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (App (exp'24, exp'25)) =>
           (Abt.Oper (ExpCmdOps.Exp'App ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'24 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'25 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Let (exp'26, (exp'27, exp'28))) =>
           (Abt.Oper (ExpCmdOps.Exp'Let ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'26 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'27
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'28 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | Triv => (Abt.Oper ExpCmdOps.Exp'Triv)
       | (Pair (exp'29, exp'30)) =>
           (Abt.Oper (ExpCmdOps.Exp'Pair ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'29 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'30 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Split (exp'31, ((exp'32, exp'33), exp'34))) =>
           (Abt.Oper (ExpCmdOps.Exp'Split ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'31 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val var = exp'32
                                           in ((Temp.toUserString var), [var])
                                           end
                        val (t1, vars'1) = let val var = exp'33
                                           in ((Temp.toUserString var), [var])
                                           end
                      in
                        ((t0, t1), (vars'0 @ vars'1))
                      end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'34 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Inl (typ'35, exp'36)) =>
           (Abt.Oper (ExpCmdOps.Exp'Inl ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'35 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'36 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Inr (typ'37, exp'38)) =>
           (Abt.Oper (ExpCmdOps.Exp'Inr ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'37 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'38 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
           (Abt.Oper (ExpCmdOps.Exp'Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'39 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'40
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'41 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val var = exp'42
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.exp_oper_bind
                                 (Abt.Binding (x, acc)))) exp'43 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Abort (typ'44, exp'45)) =>
           (Abt.Oper (ExpCmdOps.Exp'Abort ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'44 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'45 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Fold (typ'46, exp'47)) =>
           (Abt.Oper (ExpCmdOps.Exp'Fold ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'46 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'47 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Unfold exp'48) =>
           (Abt.Oper (ExpCmdOps.Exp'Unfold ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'48 vars)
              , []
              ))))
       | (Cmd cmd'49) =>
           (Abt.Oper (ExpCmdOps.Exp'Cmd ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.cmd_oper_bind (Abt.Binding (x, acc))))
                   cmd'49 vars)
              , []
              ))))
       | (ChnRef chan'50) =>
           (Abt.Oper (ExpCmdOps.Exp'ChnRef ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into (fn _ => (fn _ => (fn t => t)))
                         (Abt.Binding (x, acc))))
                   (Abt.into (fn _ => (fn _ => (fn t => t))) (Abt.Var chan'50))
                   vars)
              , []
              )))))

    fun oper_view_out vars exp =
      (case exp of
         (ExpCmdOps.Exp'String string'1) =>
           (String ((fn (x, _) => x) (string'1, [])))
       | (ExpCmdOps.Exp'Num int'2) => (Num ((fn (x, _) => x) (int'2, [])))
       | (ExpCmdOps.Exp'Succ exp'3) =>
           (Succ ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'3 vars)
              , []
              )))
       | (ExpCmdOps.Exp'Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
           (Ifz ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
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
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'5 vars)
                  , []
                  )
                val (t2, vars'2) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'6)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
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
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ExpCmdOps.Exp'Bool bool'8) => (Bool ((fn (x, _) => x) (bool'8, [])))
       | (ExpCmdOps.Exp'Negate exp'9) =>
           (Negate ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'9 vars)
              , []
              )))
       | (ExpCmdOps.Exp'If (exp'10, (exp'11, exp'12))) =>
           (If ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'10 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t0, vars'0) =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
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
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'12 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Binop (oper'13, exp'14, exp'15)) =>
           (Binop ((fn (x, _) => x)
              let
                val (t0, vars'0) = (oper'13, [])
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'14 vars)
                  , []
                  )
                val (t2, vars'2) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'15 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ExpCmdOps.Exp'Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
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
                              end) typ'16 vars)
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
                              end) typ'17 vars)
                      , []
                      )
                  in
                    ((t0, t1), (vars'0 @ vars'1))
                  end
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val x = (Temp.new exp'18)
                                           in (x, [x])
                                           end
                        val (t1, vars'1) = let val x = (Temp.new exp'19)
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
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'20 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Lam (typ'21, (exp'22, exp'23))) =>
           (Lam ((fn (x, _) => x)
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
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'23 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'App (exp'24, exp'25)) =>
           (App ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'24 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'25 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Let (exp'26, (exp'27, exp'28))) =>
           (Let ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'26 vars)
                  , []
                  )
                val (t1, vars'1) =
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
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'28 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | ExpCmdOps.Exp'Triv => Triv
       | (ExpCmdOps.Exp'Pair (exp'29, exp'30)) =>
           (Pair ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'29 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'30 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Split (exp'31, ((exp'32, exp'33), exp'34))) =>
           (Split ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'31 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') =
                      let
                        val (t0, vars'0) = let val x = (Temp.new exp'32)
                                           in (x, [x])
                                           end
                        val (t1, vars'1) = let val x = (Temp.new exp'33)
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
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'34 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Inl (typ'35, exp'36)) =>
           (Inl ((fn (x, _) => x)
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
                          end) typ'35 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'36 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Inr (typ'37, exp'38)) =>
           (Inr ((fn (x, _) => x)
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
                          end) typ'37 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'38 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
           (Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'39 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'40)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) exp'41 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
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
                                  (Abt.out ExpCmdOps.exp_oper_unbind
                                     (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1
                                        acc))
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
              end))
       | (ExpCmdOps.Exp'Abort (typ'44, exp'45)) =>
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
                          end) typ'44 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'45 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Fold (typ'46, exp'47)) =>
           (Fold ((fn (x, _) => x)
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
                          end) typ'46 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'47 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Exp'Unfold exp'48) =>
           (Unfold ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'48 vars)
              , []
              )))
       | (ExpCmdOps.Exp'Cmd cmd'49) =>
           (Cmd ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.cmd_oper_unbind
                             (Abt.unbind ExpCmdOps.cmd_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) cmd'49 vars)
              , []
              )))
       | (ExpCmdOps.Exp'ChnRef chan'50) =>
           (ChnRef ((fn (x, _) => x)
              let
                val (Abt.Var t) = (Abt.out (fn _ => (fn _ => (fn t => t)))
                  (List.foldr
                     (fn (x, acc) =>
                        let
                          val (Abt.Binding (_, acc')) =
                            (Abt.out (fn _ => (fn _ => (fn t => t)))
                               (Abt.unbind (fn _ => (fn _ => (fn t => t))) x ~1
                                  acc))
                        in
                          acc'
                        end) chan'50 vars))
              in
                (t, [])
              end)))

    fun view_out t =
      (case t of
         (Abt.Var x) => (Var x)
       | (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ExpCmdOps.exp_oper_bind (view_in [] v))
    fun out exp =
      (view_out (Abt.out ExpCmdOps.exp_oper_unbind exp))

    val Var' = (into o Var)
    val String' = (into o String)
    val Num' = (into o Num)
    val Succ' = (into o Succ)
    val Ifz' = (into o Ifz)
    val Bool' = (into o Bool)
    val Negate' = (into o Negate)
    val If' = (into o If)
    val Binop' = (into o Binop)
    val Fun' = (into o Fun)
    val Lam' = (into o Lam)
    val App' = (into o App)
    val Let' = (into o Let)
    val Triv' = (into Triv)
    val Pair' = (into o Pair)
    val Split' = (into o Split)
    val Inl' = (into o Inl)
    val Inr' = (into o Inr)
    val Case' = (into o Case)
    val Abort' = (into o Abort)
    val Fold' = (into o Fold)
    val Unfold' = (into o Unfold)
    val Cmd' = (into o Cmd)
    val ChnRef' = (into o ChnRef)
  end

  structure Cmd =
  struct
    type exp = ExpCmdOps.exp
    type expVar = Temp.t
    type cmd = ExpCmdOps.cmd
    type t = cmd

    datatype view =
      Ret of exp
    | Bind of exp * (expVar * cmd)
    | Spawn of exp
    | Emit of exp * exp
    | Sync of exp
    | NewChn of Typ.t * (Chan.t * cmd)
    | Print of exp

    fun view_in vars cmd =
      (case cmd of
         (Ret exp'1) =>
           (Abt.Oper (ExpCmdOps.Cmd'Ret ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'1 vars)
              , []
              ))))
       | (Bind (exp'2, (exp'3, cmd'4))) =>
           (Abt.Oper (ExpCmdOps.Cmd'Bind ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = exp'3
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.cmd_oper_bind
                                 (Abt.Binding (x, acc)))) cmd'4 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Spawn exp'5) =>
           (Abt.Oper (ExpCmdOps.Cmd'Spawn ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'5 vars)
              , []
              ))))
       | (Emit (exp'6, exp'7)) =>
           (Abt.Oper (ExpCmdOps.Cmd'Emit ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'6 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpCmdOps.exp_oper_bind
                             (Abt.Binding (x, acc)))) exp'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Sync exp'8) =>
           (Abt.Oper (ExpCmdOps.Cmd'Sync ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'8 vars)
              , []
              ))))
       | (NewChn (typ'9, (chan'10, cmd'11))) =>
           (Abt.Oper (ExpCmdOps.Cmd'NewChn ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'9 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val var = chan'10
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into ExpCmdOps.cmd_oper_bind
                                 (Abt.Binding (x, acc)))) cmd'11 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Print exp'12) =>
           (Abt.Oper (ExpCmdOps.Cmd'Print ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpCmdOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'12 vars)
              , []
              )))))

    fun oper_view_out vars cmd =
      (case cmd of
         (ExpCmdOps.Cmd'Ret exp'1) =>
           (Ret ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'1 vars)
              , []
              )))
       | (ExpCmdOps.Cmd'Bind (exp'2, (exp'3, cmd'4))) =>
           (Bind ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'2 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (t, vars') = let val x = (Temp.new exp'3)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpCmdOps.cmd_oper_unbind
                                     (Abt.unbind ExpCmdOps.cmd_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) cmd'4 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Cmd'Spawn exp'5) =>
           (Spawn ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'5 vars)
              , []
              )))
       | (ExpCmdOps.Cmd'Emit (exp'6, exp'7)) =>
           (Emit ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
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
                              (Abt.out ExpCmdOps.exp_oper_unbind
                                 (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'7 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Cmd'Sync exp'8) =>
           (Sync ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'8 vars)
              , []
              )))
       | (ExpCmdOps.Cmd'NewChn (typ'9, (chan'10, cmd'11))) =>
           (NewChn ((fn (x, _) => x)
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
                  let
                    val (t, vars') = let val x = (Temp.new chan'10)
                                     in (x, [x])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldr
                           (fn (x, acc) =>
                              let
                                val (Abt.Binding (_, acc')) =
                                  (Abt.out ExpCmdOps.cmd_oper_unbind
                                     (Abt.unbind ExpCmdOps.cmd_oper_unbind x ~1
                                        acc))
                              in
                                acc'
                              end) cmd'11 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpCmdOps.Cmd'Print exp'12) =>
           (Print ((fn (x, _) => x)
              ( (List.foldr
                   (fn (x, acc) =>
                      let
                        val (Abt.Binding (_, acc')) =
                          (Abt.out ExpCmdOps.exp_oper_unbind
                             (Abt.unbind ExpCmdOps.exp_oper_unbind x ~1 acc))
                      in
                        acc'
                      end) exp'12 vars)
              , []
              ))))

    fun view_out t =
      (case t of
         (Abt.Oper oper) => (oper_view_out [] oper)
       | _ => (raise Fail "Internal Abbot Error"))

    fun into v =
      (Abt.into ExpCmdOps.cmd_oper_bind (view_in [] v))
    fun out cmd =
      (view_out (Abt.out ExpCmdOps.cmd_oper_unbind cmd))

    val Ret' = (into o Ret)
    val Bind' = (into o Bind)
    val Spawn' = (into o Spawn)
    val Emit' = (into o Emit)
    val Sync' = (into o Sync)
    val NewChn' = (into o NewChn)
    val Print' = (into o Print)
  end

  fun exp_toString exp =
    (case (Exp.out exp) of
       (Exp.Var x) => (Exp.Var.toString x)
     | (Exp.String string'1) => ("(String " ^ (String.toString string'1) ^ ")")
     | (Exp.Num int'2) => ("(Num " ^ (Int.toString int'2) ^ ")")
     | (Exp.Succ exp'3) => ("(Succ " ^ (exp_toString exp'3) ^ ")")
     | (Exp.Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
         ("(Ifz "
          ^
          ("(" ^ (exp_toString exp'4) ^ ", " ^ (exp_toString exp'5) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'6) ^ " . " ^ (exp_toString exp'7) ^ ")")
           ^ ")") ^ ")")
     | (Exp.Bool bool'8) => ("(Bool " ^ (Bool.toString bool'8) ^ ")")
     | (Exp.Negate exp'9) => ("(Negate " ^ (exp_toString exp'9) ^ ")")
     | (Exp.If (exp'10, (exp'11, exp'12))) =>
         ("(If "
          ^
          ("(" ^ (exp_toString exp'10) ^ ", "
           ^ ("(" ^ (exp_toString exp'11) ^ ", " ^ (exp_toString exp'12) ^ ")")
           ^ ")") ^ ")")
     | (Exp.Binop (oper'13, exp'14, exp'15)) =>
         ("(Binop "
          ^
          ("(" ^ (Oper.toString oper'13) ^ ", " ^ (exp_toString exp'14) ^ ", "
           ^ (exp_toString exp'15) ^ ")") ^ ")")
     | (Exp.Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
         ("(Fun "
          ^
          ("("
           ^ ("(" ^ (Typ.toString typ'16) ^ ", " ^ (Typ.toString typ'17) ^ ")")
           ^ ", "
           ^
           ("("
            ^
            ("(" ^ (Exp.Var.toString exp'18) ^ ", " ^ (Exp.Var.toString exp'19)
             ^ ")") ^ " . " ^ (exp_toString exp'20) ^ ")") ^ ")") ^ ")")
     | (Exp.Lam (typ'21, (exp'22, exp'23))) =>
         ("(Lam "
          ^
          ("(" ^ (Typ.toString typ'21) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'22) ^ " . " ^ (exp_toString exp'23)
            ^ ")") ^ ")") ^ ")")
     | (Exp.App (exp'24, exp'25)) =>
         ("(App "
          ^ ("(" ^ (exp_toString exp'24) ^ ", " ^ (exp_toString exp'25) ^ ")")
          ^ ")")
     | (Exp.Let (exp'26, (exp'27, exp'28))) =>
         ("(Let "
          ^
          ("(" ^ (exp_toString exp'26) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'27) ^ " . " ^ (exp_toString exp'28)
            ^ ")") ^ ")") ^ ")")
     | Exp.Triv => "Triv"
     | (Exp.Pair (exp'29, exp'30)) =>
         ("(Pair "
          ^ ("(" ^ (exp_toString exp'29) ^ ", " ^ (exp_toString exp'30) ^ ")")
          ^ ")")
     | (Exp.Split (exp'31, ((exp'32, exp'33), exp'34))) =>
         ("(Split "
          ^
          ("(" ^ (exp_toString exp'31) ^ ", "
           ^
           ("("
            ^
            ("(" ^ (Exp.Var.toString exp'32) ^ ", " ^ (Exp.Var.toString exp'33)
             ^ ")") ^ " . " ^ (exp_toString exp'34) ^ ")") ^ ")") ^ ")")
     | (Exp.Inl (typ'35, exp'36)) =>
         ("(Inl "
          ^ ("(" ^ (Typ.toString typ'35) ^ ", " ^ (exp_toString exp'36) ^ ")")
          ^ ")")
     | (Exp.Inr (typ'37, exp'38)) =>
         ("(Inr "
          ^ ("(" ^ (Typ.toString typ'37) ^ ", " ^ (exp_toString exp'38) ^ ")")
          ^ ")")
     | (Exp.Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
         ("(Case "
          ^
          ("(" ^ (exp_toString exp'39) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'40) ^ " . " ^ (exp_toString exp'41)
            ^ ")") ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'42) ^ " . " ^ (exp_toString exp'43)
            ^ ")") ^ ")") ^ ")")
     | (Exp.Abort (typ'44, exp'45)) =>
         ("(Abort "
          ^ ("(" ^ (Typ.toString typ'44) ^ ", " ^ (exp_toString exp'45) ^ ")")
          ^ ")")
     | (Exp.Fold (typ'46, exp'47)) =>
         ("(Fold "
          ^ ("(" ^ (Typ.toString typ'46) ^ ", " ^ (exp_toString exp'47) ^ ")")
          ^ ")")
     | (Exp.Unfold exp'48) => ("(Unfold " ^ (exp_toString exp'48) ^ ")")
     | (Exp.Cmd cmd'49) => ("(Cmd " ^ (cmd_toString cmd'49) ^ ")")
     | (Exp.ChnRef chan'50) => ("(ChnRef " ^ (Chan.toString chan'50) ^ ")"))
  and cmd_toString cmd =
    (case (Cmd.out cmd) of
       (Cmd.Ret exp'1) => ("(Ret " ^ (exp_toString exp'1) ^ ")")
     | (Cmd.Bind (exp'2, (exp'3, cmd'4))) =>
         ("(Bind "
          ^
          ("(" ^ (exp_toString exp'2) ^ ", "
           ^
           ("(" ^ (Exp.Var.toString exp'3) ^ " . " ^ (cmd_toString cmd'4) ^ ")")
           ^ ")") ^ ")")
     | (Cmd.Spawn exp'5) => ("(Spawn " ^ (exp_toString exp'5) ^ ")")
     | (Cmd.Emit (exp'6, exp'7)) =>
         ("(Emit "
          ^ ("(" ^ (exp_toString exp'6) ^ ", " ^ (exp_toString exp'7) ^ ")")
          ^ ")")
     | (Cmd.Sync exp'8) => ("(Sync " ^ (exp_toString exp'8) ^ ")")
     | (Cmd.NewChn (typ'9, (chan'10, cmd'11))) =>
         ("(NewChn "
          ^
          ("(" ^ (Typ.toString typ'9) ^ ", "
           ^
           ("(" ^ (Chan.toString chan'10) ^ " . " ^ (cmd_toString cmd'11) ^ ")")
           ^ ")") ^ ")")
     | (Cmd.Print exp'12) => ("(Print " ^ (exp_toString exp'12) ^ ")"))

  fun exp_substTyp t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (Exp.Var' y)
     | (Exp.String string'1) => (Exp.String' string'1)
     | (Exp.Num int'2) => (Exp.Num' int'2)
     | (Exp.Succ exp'3) => (Exp.Succ' (exp_substTyp t x exp'3))
     | (Exp.Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
         (Exp.Ifz'
            ( (exp_substTyp t x exp'4)
            , (exp_substTyp t x exp'5)
            , (exp'6, (exp_substTyp t x exp'7))
            ))
     | (Exp.Bool bool'8) => (Exp.Bool' bool'8)
     | (Exp.Negate exp'9) => (Exp.Negate' (exp_substTyp t x exp'9))
     | (Exp.If (exp'10, (exp'11, exp'12))) =>
         (Exp.If'
            ( (exp_substTyp t x exp'10)
            , ((exp_substTyp t x exp'11), (exp_substTyp t x exp'12))
            ))
     | (Exp.Binop (oper'13, exp'14, exp'15)) =>
         (Exp.Binop'
            (oper'13, (exp_substTyp t x exp'14), (exp_substTyp t x exp'15)))
     | (Exp.Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
         (Exp.Fun'
            ( ((Typ.subst t x typ'16), (Typ.subst t x typ'17))
            , ((exp'18, exp'19), (exp_substTyp t x exp'20))
            ))
     | (Exp.Lam (typ'21, (exp'22, exp'23))) =>
         (Exp.Lam' ((Typ.subst t x typ'21), (exp'22, (exp_substTyp t x exp'23))))
     | (Exp.App (exp'24, exp'25)) =>
         (Exp.App' ((exp_substTyp t x exp'24), (exp_substTyp t x exp'25)))
     | (Exp.Let (exp'26, (exp'27, exp'28))) =>
         (Exp.Let'
            ((exp_substTyp t x exp'26), (exp'27, (exp_substTyp t x exp'28))))
     | Exp.Triv => Exp.Triv'
     | (Exp.Pair (exp'29, exp'30)) =>
         (Exp.Pair' ((exp_substTyp t x exp'29), (exp_substTyp t x exp'30)))
     | (Exp.Split (exp'31, ((exp'32, exp'33), exp'34))) =>
         (Exp.Split'
            ( (exp_substTyp t x exp'31)
            , ((exp'32, exp'33), (exp_substTyp t x exp'34))
            ))
     | (Exp.Inl (typ'35, exp'36)) =>
         (Exp.Inl' ((Typ.subst t x typ'35), (exp_substTyp t x exp'36)))
     | (Exp.Inr (typ'37, exp'38)) =>
         (Exp.Inr' ((Typ.subst t x typ'37), (exp_substTyp t x exp'38)))
     | (Exp.Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
         (Exp.Case'
            ( (exp_substTyp t x exp'39)
            , (exp'40, (exp_substTyp t x exp'41))
            , (exp'42, (exp_substTyp t x exp'43))
            ))
     | (Exp.Abort (typ'44, exp'45)) =>
         (Exp.Abort' ((Typ.subst t x typ'44), (exp_substTyp t x exp'45)))
     | (Exp.Fold (typ'46, exp'47)) =>
         (Exp.Fold' ((Typ.subst t x typ'46), (exp_substTyp t x exp'47)))
     | (Exp.Unfold exp'48) => (Exp.Unfold' (exp_substTyp t x exp'48))
     | (Exp.Cmd cmd'49) => (Exp.Cmd' (cmd_substTyp t x cmd'49))
     | (Exp.ChnRef chan'50) => (Exp.ChnRef' chan'50))
  and exp_subst t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (if (Exp.Var.equal (x, y)) then t else (Exp.Var' y))
     | (Exp.String string'1) => (Exp.String' string'1)
     | (Exp.Num int'2) => (Exp.Num' int'2)
     | (Exp.Succ exp'3) => (Exp.Succ' (exp_subst t x exp'3))
     | (Exp.Ifz (exp'4, exp'5, (exp'6, exp'7))) =>
         (Exp.Ifz'
            ( (exp_subst t x exp'4)
            , (exp_subst t x exp'5)
            , (exp'6, (exp_subst t x exp'7))
            ))
     | (Exp.Bool bool'8) => (Exp.Bool' bool'8)
     | (Exp.Negate exp'9) => (Exp.Negate' (exp_subst t x exp'9))
     | (Exp.If (exp'10, (exp'11, exp'12))) =>
         (Exp.If'
            ( (exp_subst t x exp'10)
            , ((exp_subst t x exp'11), (exp_subst t x exp'12))
            ))
     | (Exp.Binop (oper'13, exp'14, exp'15)) =>
         (Exp.Binop' (oper'13, (exp_subst t x exp'14), (exp_subst t x exp'15)))
     | (Exp.Fun ((typ'16, typ'17), ((exp'18, exp'19), exp'20))) =>
         (Exp.Fun'
            ((typ'16, typ'17), ((exp'18, exp'19), (exp_subst t x exp'20))))
     | (Exp.Lam (typ'21, (exp'22, exp'23))) =>
         (Exp.Lam' (typ'21, (exp'22, (exp_subst t x exp'23))))
     | (Exp.App (exp'24, exp'25)) =>
         (Exp.App' ((exp_subst t x exp'24), (exp_subst t x exp'25)))
     | (Exp.Let (exp'26, (exp'27, exp'28))) =>
         (Exp.Let' ((exp_subst t x exp'26), (exp'27, (exp_subst t x exp'28))))
     | Exp.Triv => Exp.Triv'
     | (Exp.Pair (exp'29, exp'30)) =>
         (Exp.Pair' ((exp_subst t x exp'29), (exp_subst t x exp'30)))
     | (Exp.Split (exp'31, ((exp'32, exp'33), exp'34))) =>
         (Exp.Split'
            ((exp_subst t x exp'31), ((exp'32, exp'33), (exp_subst t x exp'34))))
     | (Exp.Inl (typ'35, exp'36)) => (Exp.Inl' (typ'35, (exp_subst t x exp'36)))
     | (Exp.Inr (typ'37, exp'38)) => (Exp.Inr' (typ'37, (exp_subst t x exp'38)))
     | (Exp.Case (exp'39, (exp'40, exp'41), (exp'42, exp'43))) =>
         (Exp.Case'
            ( (exp_subst t x exp'39)
            , (exp'40, (exp_subst t x exp'41))
            , (exp'42, (exp_subst t x exp'43))
            ))
     | (Exp.Abort (typ'44, exp'45)) =>
         (Exp.Abort' (typ'44, (exp_subst t x exp'45)))
     | (Exp.Fold (typ'46, exp'47)) =>
         (Exp.Fold' (typ'46, (exp_subst t x exp'47)))
     | (Exp.Unfold exp'48) => (Exp.Unfold' (exp_subst t x exp'48))
     | (Exp.Cmd cmd'49) => (Exp.Cmd' (cmd_substExp t x cmd'49))
     | (Exp.ChnRef chan'50) => (Exp.ChnRef' chan'50))
  and cmd_substTyp t x cmd =
    (case (Cmd.out cmd) of
       (Cmd.Ret exp'1) => (Cmd.Ret' (exp_substTyp t x exp'1))
     | (Cmd.Bind (exp'2, (exp'3, cmd'4))) =>
         (Cmd.Bind'
            ((exp_substTyp t x exp'2), (exp'3, (cmd_substTyp t x cmd'4))))
     | (Cmd.Spawn exp'5) => (Cmd.Spawn' (exp_substTyp t x exp'5))
     | (Cmd.Emit (exp'6, exp'7)) =>
         (Cmd.Emit' ((exp_substTyp t x exp'6), (exp_substTyp t x exp'7)))
     | (Cmd.Sync exp'8) => (Cmd.Sync' (exp_substTyp t x exp'8))
     | (Cmd.NewChn (typ'9, (chan'10, cmd'11))) =>
         (Cmd.NewChn'
            ((Typ.subst t x typ'9), (chan'10, (cmd_substTyp t x cmd'11))))
     | (Cmd.Print exp'12) => (Cmd.Print' (exp_substTyp t x exp'12)))
  and cmd_substExp t x cmd =
    (case (Cmd.out cmd) of
       (Cmd.Ret exp'1) => (Cmd.Ret' (exp_subst t x exp'1))
     | (Cmd.Bind (exp'2, (exp'3, cmd'4))) =>
         (Cmd.Bind' ((exp_subst t x exp'2), (exp'3, (cmd_substExp t x cmd'4))))
     | (Cmd.Spawn exp'5) => (Cmd.Spawn' (exp_subst t x exp'5))
     | (Cmd.Emit (exp'6, exp'7)) =>
         (Cmd.Emit' ((exp_subst t x exp'6), (exp_subst t x exp'7)))
     | (Cmd.Sync exp'8) => (Cmd.Sync' (exp_subst t x exp'8))
     | (Cmd.NewChn (typ'9, (chan'10, cmd'11))) =>
         (Cmd.NewChn' (typ'9, (chan'10, (cmd_substExp t x cmd'11))))
     | (Cmd.Print exp'12) => (Cmd.Print' (exp_subst t x exp'12)))

  structure Exp =
  struct
    open Exp
    val toString = exp_toString
    val aequiv = (Abt.aequiv ExpCmdOps.exp_oper_aequiv)
    val substTyp = exp_substTyp
    val subst = exp_subst
  end

  structure Cmd =
  struct
    open Cmd
    val toString = cmd_toString
    val aequiv = (Abt.aequiv ExpCmdOps.cmd_oper_aequiv)
    val substTyp = cmd_substTyp
    val substExp = cmd_substExp
  end
end

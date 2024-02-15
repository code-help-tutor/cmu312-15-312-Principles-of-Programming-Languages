functor FPC
  (structure Int:
   sig
     type t
     val equal: t * t -> bool
     val toString: t -> string
   end
   structure Label:
   sig
     type t
     val equal: t * t -> bool
     val toString: t -> string
   end
   structure Labeled:
   sig
     type 'a t
     val equal: ('a * 'a -> bool) -> 'a t * 'a t -> bool
     val toString: ('a -> string) -> 'a t -> string

     val iter: ('a1 * 'state -> 'a2 * 'state)
               -> 'a1 t * 'state
               -> 'a2 t * 'state

     include DICT where type key = Label.t and type 'a dict = 'a t
   end) :> FPC
           where type 'a Labeled.t = 'a Labeled.t
           where type Label.t = Label.t
           where type Int.t = Int.t =
struct

  structure Labeled = Labeled

  structure Label = Label

  structure Int = Int

  structure TypOps =
  struct
    datatype typ_oper =
      Typ'Int
    | Typ'List of typ
    | Typ'Prod of typ Labeled.t
    | Typ'Sum of typ Labeled.t
    | Typ'Arrow of typ * typ
    | Typ'Rec of string * typ
    withtype typ = typ_oper Abt.t

    fun typ_oper_bind x i typ =
      (case typ of
         Typ'Int => Typ'Int
       | (Typ'List typ'1) => (Typ'List (Abt.bind typ_oper_bind x i typ'1))
       | (Typ'Prod labeled'3) =>
           (Typ'Prod ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'2, ()) => ((Abt.bind typ_oper_bind x i typ'2), ()))
                 (labeled'3, ()))))
       | (Typ'Sum labeled'5) =>
           (Typ'Sum ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'4, ()) => ((Abt.bind typ_oper_bind x i typ'4), ()))
                 (labeled'5, ()))))
       | (Typ'Arrow (typ'6, typ'7)) =>
           (Typ'Arrow
              ( (Abt.bind typ_oper_bind x i typ'6)
              , (Abt.bind typ_oper_bind x i typ'7)
              ))
       | (Typ'Rec (typ'8, typ'9)) =>
           (Typ'Rec (typ'8, (Abt.bind typ_oper_bind x i typ'9))))

    fun typ_oper_unbind x i typ =
      (case typ of
         Typ'Int => Typ'Int
       | (Typ'List typ'1) => (Typ'List (Abt.unbind typ_oper_unbind x i typ'1))
       | (Typ'Prod labeled'3) =>
           (Typ'Prod ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'2, ()) => ((Abt.unbind typ_oper_unbind x i typ'2), ()))
                 (labeled'3, ()))))
       | (Typ'Sum labeled'5) =>
           (Typ'Sum ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'4, ()) => ((Abt.unbind typ_oper_unbind x i typ'4), ()))
                 (labeled'5, ()))))
       | (Typ'Arrow (typ'6, typ'7)) =>
           (Typ'Arrow
              ( (Abt.unbind typ_oper_unbind x i typ'6)
              , (Abt.unbind typ_oper_unbind x i typ'7)
              ))
       | (Typ'Rec (typ'8, typ'9)) =>
           (Typ'Rec (typ'8, (Abt.unbind typ_oper_unbind x i typ'9))))

    fun typ_oper_aequiv (typ1, typ2) =
      (case (typ1, typ2) of
         (Typ'Int, Typ'Int) => true
       | ((Typ'List typ'1), (Typ'List typ'2)) =>
           ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
       | ((Typ'Prod labeled'3), (Typ'Prod labeled'4)) =>
           (Labeled.equal
              (fn (typ'1, typ'2) =>
                 ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2)))
              (labeled'3, labeled'4))
       | ((Typ'Sum labeled'3), (Typ'Sum labeled'4)) =>
           (Labeled.equal
              (fn (typ'1, typ'2) =>
                 ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2)))
              (labeled'3, labeled'4))
       | ((Typ'Arrow (typ'1, typ'3)), (Typ'Arrow (typ'2, typ'4))) =>
           (((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2))
            andalso ((Abt.aequiv typ_oper_aequiv) (typ'3, typ'4)))
       | ((Typ'Rec (_, typ'1)), (Typ'Rec (_, typ'2))) =>
           (true andalso ((Abt.aequiv typ_oper_aequiv) (typ'1, typ'2)))
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
    | Int
    | List of typ
    | Prod of typ Labeled.t
    | Sum of typ Labeled.t
    | Arrow of typ * typ
    | Rec of typVar * typ

    fun view_in vars typ =
      (case typ of
         (Var x) => (Abt.Var x)
       | Int => (Abt.Oper TypOps.Typ'Int)
       | (List typ'1) =>
           (Abt.Oper (TypOps.Typ'List ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'1 vars)
              , []
              ))))
       | (Prod labeled'3) =>
           (Abt.Oper (TypOps.Typ'Prod ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'2, vars') =>
                    let
                      val (t, vars'') =
                        ( (List.foldl
                             (fn (x, acc) =>
                                (Abt.into TypOps.typ_oper_bind
                                   (Abt.Binding (x, acc)))) typ'2 vars)
                        , []
                        )
                    in
                      (t, (vars'' @ vars'))
                    end) (labeled'3, [])))))
       | (Sum labeled'5) =>
           (Abt.Oper (TypOps.Typ'Sum ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'4, vars') =>
                    let
                      val (t, vars'') =
                        ( (List.foldl
                             (fn (x, acc) =>
                                (Abt.into TypOps.typ_oper_bind
                                   (Abt.Binding (x, acc)))) typ'4 vars)
                        , []
                        )
                    in
                      (t, (vars'' @ vars'))
                    end) (labeled'5, [])))))
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
       | (Rec (typ'8, typ'9)) =>
           (Abt.Oper (TypOps.Typ'Rec ((fn (x, _) => x)
              let
                val (t, vars') = let val var = typ'8
                                 in ((Temp.toUserString var), [var])
                                 end
                val vars = (vars' @ vars)
                val (t', vars') =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'9 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end))))

    fun oper_view_out vars typ =
      (case typ of
         TypOps.Typ'Int => Int
       | (TypOps.Typ'List typ'1) =>
           (List ((fn (x, _) => x)
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
       | (TypOps.Typ'Prod labeled'3) =>
           (Prod ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'2, vars') =>
                    let
                      val (t, vars'') =
                        ( (List.foldr
                             (fn (x, acc) =>
                                let
                                  val (Abt.Binding (_, acc')) =
                                    (Abt.out TypOps.typ_oper_unbind
                                       (Abt.unbind TypOps.typ_oper_unbind x ~1
                                          acc))
                                in
                                  acc'
                                end) typ'2 vars)
                        , []
                        )
                    in
                      (t, (vars'' @ vars'))
                    end) (labeled'3, []))))
       | (TypOps.Typ'Sum labeled'5) =>
           (Sum ((fn (x, _) => x)
              (Labeled.iter
                 (fn (typ'4, vars') =>
                    let
                      val (t, vars'') =
                        ( (List.foldr
                             (fn (x, acc) =>
                                let
                                  val (Abt.Binding (_, acc')) =
                                    (Abt.out TypOps.typ_oper_unbind
                                       (Abt.unbind TypOps.typ_oper_unbind x ~1
                                          acc))
                                in
                                  acc'
                                end) typ'4 vars)
                        , []
                        )
                    in
                      (t, (vars'' @ vars'))
                    end) (labeled'5, []))))
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
       | (TypOps.Typ'Rec (typ'8, typ'9)) =>
           (Rec ((fn (x, _) => x)
              let
                val (t, vars') = let val x = (Temp.new typ'8) in (x, [x]) end
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
                          end) typ'9 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end)))

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
    val Int' = (into Int)
    val List' = (into o List)
    val Prod' = (into o Prod)
    val Sum' = (into o Sum)
    val Arrow' = (into o Arrow)
    val Rec' = (into o Rec)
  end

  fun typ_toString typ =
    (case (Typ.out typ) of
       (Typ.Var x) => (Typ.Var.toString x)
     | Typ.Int => "Int"
     | (Typ.List typ'1) => ("(List " ^ (typ_toString typ'1) ^ ")")
     | (Typ.Prod labeled'3) =>
         ("(Prod "
          ^ (Labeled.toString (fn typ'2 => (typ_toString typ'2)) labeled'3)
          ^ ")")
     | (Typ.Sum labeled'5) =>
         ("(Sum "
          ^ (Labeled.toString (fn typ'4 => (typ_toString typ'4)) labeled'5)
          ^ ")")
     | (Typ.Arrow (typ'6, typ'7)) =>
         ("(Arrow "
          ^ ("(" ^ (typ_toString typ'6) ^ ", " ^ (typ_toString typ'7) ^ ")")
          ^ ")")
     | (Typ.Rec (typ'8, typ'9)) =>
         ("(Rec "
          ^
          ("(" ^ (Typ.Var.toString typ'8) ^ " . " ^ (typ_toString typ'9) ^ ")")
          ^ ")"))

  fun typ_subst t x typ =
    (case (Typ.out typ) of
       (Typ.Var y) => (if (Typ.Var.equal (x, y)) then t else (Typ.Var' y))
     | Typ.Int => Typ.Int'
     | (Typ.List typ'1) => (Typ.List' (typ_subst t x typ'1))
     | (Typ.Prod labeled'3) =>
         (Typ.Prod' (Labeled.map (fn typ'2 => (typ_subst t x typ'2)) labeled'3))
     | (Typ.Sum labeled'5) =>
         (Typ.Sum' (Labeled.map (fn typ'4 => (typ_subst t x typ'4)) labeled'5))
     | (Typ.Arrow (typ'6, typ'7)) =>
         (Typ.Arrow' ((typ_subst t x typ'6), (typ_subst t x typ'7)))
     | (Typ.Rec (typ'8, typ'9)) => (Typ.Rec' (typ'8, (typ_subst t x typ'9))))

  structure Typ =
  struct
    open Typ
    val toString = typ_toString
    val aequiv = (Abt.aequiv TypOps.typ_oper_aequiv)
    val subst = typ_subst
  end

  structure ExpOps =
  struct
    datatype exp_oper =
      Exp'Error of Typ.t
    | Exp'Int of Int.t
    | Exp'Plus of exp * exp
    | Exp'LEq of exp * exp
    | Exp'List of Typ.t * exp list
    | Exp'Append of exp * exp
    | Exp'Index of exp * exp
    | Exp'Len of exp
    | Exp'Pair of exp Labeled.t
    | Exp'Proj of exp * Label.t
    | Exp'Inj of Typ.t Labeled.t * Label.t * exp
    | Exp'Case of Typ.t * exp * (string * exp) Labeled.t
    | Exp'Lam of (string * Typ.t) * exp
    | Exp'Ap of exp * exp
    | Exp'Fold of (string * Typ.t) * exp
    | Exp'Unfold of exp
    withtype exp = exp_oper Abt.t

    fun exp_oper_bind x i exp =
      (case exp of
         (Exp'Error typ'1) =>
           (Exp'Error (Abt.bind TypOps.typ_oper_bind x i typ'1))
       | (Exp'Int int'2) => (Exp'Int int'2)
       | (Exp'Plus (exp'3, exp'4)) =>
           (Exp'Plus
              ( (Abt.bind exp_oper_bind x i exp'3)
              , (Abt.bind exp_oper_bind x i exp'4)
              ))
       | (Exp'LEq (exp'5, exp'6)) =>
           (Exp'LEq
              ( (Abt.bind exp_oper_bind x i exp'5)
              , (Abt.bind exp_oper_bind x i exp'6)
              ))
       | (Exp'List (typ'7, list'9)) =>
           (Exp'List
              ( (Abt.bind TypOps.typ_oper_bind x i typ'7)
              , (List.map (fn exp'8 => (Abt.bind exp_oper_bind x i exp'8))
                   list'9)
              ))
       | (Exp'Append (exp'10, exp'11)) =>
           (Exp'Append
              ( (Abt.bind exp_oper_bind x i exp'10)
              , (Abt.bind exp_oper_bind x i exp'11)
              ))
       | (Exp'Index (exp'12, exp'13)) =>
           (Exp'Index
              ( (Abt.bind exp_oper_bind x i exp'12)
              , (Abt.bind exp_oper_bind x i exp'13)
              ))
       | (Exp'Len exp'14) => (Exp'Len (Abt.bind exp_oper_bind x i exp'14))
       | (Exp'Pair labeled'16) =>
           (Exp'Pair ((fn (x, _) => x)
              (Labeled.iter
                 (fn (exp'15, ()) => ((Abt.bind exp_oper_bind x i exp'15), ()))
                 (labeled'16, ()))))
       | (Exp'Proj (exp'17, label'18)) =>
           (Exp'Proj ((Abt.bind exp_oper_bind x i exp'17), label'18))
       | (Exp'Inj (labeled'20, label'21, exp'22)) =>
           (Exp'Inj
              ( ((fn (x, _) => x)
                   (Labeled.iter
                      (fn (typ'19, ()) =>
                         ((Abt.bind TypOps.typ_oper_bind x i typ'19), ()))
                      (labeled'20, ())))
              , label'21
              , (Abt.bind exp_oper_bind x i exp'22)
              ))
       | (Exp'Case (typ'23, exp'24, labeled'27)) =>
           (Exp'Case
              ( (Abt.bind TypOps.typ_oper_bind x i typ'23)
              , (Abt.bind exp_oper_bind x i exp'24)
              , ((fn (x, _) => x)
                   (Labeled.iter
                      (fn ((exp'25, exp'26), ()) =>
                         ((exp'25, (Abt.bind exp_oper_bind x i exp'26)), ()))
                      (labeled'27, ())))
              ))
       | (Exp'Lam ((exp'28, typ'29), exp'30)) =>
           (Exp'Lam
              ( (exp'28, (Abt.bind TypOps.typ_oper_bind x i typ'29))
              , (Abt.bind exp_oper_bind x i exp'30)
              ))
       | (Exp'Ap (exp'31, exp'32)) =>
           (Exp'Ap
              ( (Abt.bind exp_oper_bind x i exp'31)
              , (Abt.bind exp_oper_bind x i exp'32)
              ))
       | (Exp'Fold ((typ'33, typ'34), exp'35)) =>
           (Exp'Fold
              ( (typ'33, (Abt.bind TypOps.typ_oper_bind x i typ'34))
              , (Abt.bind exp_oper_bind x i exp'35)
              ))
       | (Exp'Unfold exp'36) => (Exp'Unfold (Abt.bind exp_oper_bind x i exp'36)))

    fun exp_oper_unbind x i exp =
      (case exp of
         (Exp'Error typ'1) =>
           (Exp'Error (Abt.unbind TypOps.typ_oper_unbind x i typ'1))
       | (Exp'Int int'2) => (Exp'Int int'2)
       | (Exp'Plus (exp'3, exp'4)) =>
           (Exp'Plus
              ( (Abt.unbind exp_oper_unbind x i exp'3)
              , (Abt.unbind exp_oper_unbind x i exp'4)
              ))
       | (Exp'LEq (exp'5, exp'6)) =>
           (Exp'LEq
              ( (Abt.unbind exp_oper_unbind x i exp'5)
              , (Abt.unbind exp_oper_unbind x i exp'6)
              ))
       | (Exp'List (typ'7, list'9)) =>
           (Exp'List
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'7)
              , (List.map (fn exp'8 => (Abt.unbind exp_oper_unbind x i exp'8))
                   list'9)
              ))
       | (Exp'Append (exp'10, exp'11)) =>
           (Exp'Append
              ( (Abt.unbind exp_oper_unbind x i exp'10)
              , (Abt.unbind exp_oper_unbind x i exp'11)
              ))
       | (Exp'Index (exp'12, exp'13)) =>
           (Exp'Index
              ( (Abt.unbind exp_oper_unbind x i exp'12)
              , (Abt.unbind exp_oper_unbind x i exp'13)
              ))
       | (Exp'Len exp'14) => (Exp'Len (Abt.unbind exp_oper_unbind x i exp'14))
       | (Exp'Pair labeled'16) =>
           (Exp'Pair ((fn (x, _) => x)
              (Labeled.iter
                 (fn (exp'15, ()) =>
                    ((Abt.unbind exp_oper_unbind x i exp'15), ()))
                 (labeled'16, ()))))
       | (Exp'Proj (exp'17, label'18)) =>
           (Exp'Proj ((Abt.unbind exp_oper_unbind x i exp'17), label'18))
       | (Exp'Inj (labeled'20, label'21, exp'22)) =>
           (Exp'Inj
              ( ((fn (x, _) => x)
                   (Labeled.iter
                      (fn (typ'19, ()) =>
                         ((Abt.unbind TypOps.typ_oper_unbind x i typ'19), ()))
                      (labeled'20, ())))
              , label'21
              , (Abt.unbind exp_oper_unbind x i exp'22)
              ))
       | (Exp'Case (typ'23, exp'24, labeled'27)) =>
           (Exp'Case
              ( (Abt.unbind TypOps.typ_oper_unbind x i typ'23)
              , (Abt.unbind exp_oper_unbind x i exp'24)
              , ((fn (x, _) => x)
                   (Labeled.iter
                      (fn ((exp'25, exp'26), ()) =>
                         ((exp'25, (Abt.unbind exp_oper_unbind x i exp'26)), ()))
                      (labeled'27, ())))
              ))
       | (Exp'Lam ((exp'28, typ'29), exp'30)) =>
           (Exp'Lam
              ( (exp'28, (Abt.unbind TypOps.typ_oper_unbind x i typ'29))
              , (Abt.unbind exp_oper_unbind x i exp'30)
              ))
       | (Exp'Ap (exp'31, exp'32)) =>
           (Exp'Ap
              ( (Abt.unbind exp_oper_unbind x i exp'31)
              , (Abt.unbind exp_oper_unbind x i exp'32)
              ))
       | (Exp'Fold ((typ'33, typ'34), exp'35)) =>
           (Exp'Fold
              ( (typ'33, (Abt.unbind TypOps.typ_oper_unbind x i typ'34))
              , (Abt.unbind exp_oper_unbind x i exp'35)
              ))
       | (Exp'Unfold exp'36) =>
           (Exp'Unfold (Abt.unbind exp_oper_unbind x i exp'36)))

    fun exp_oper_aequiv (exp1, exp2) =
      (case (exp1, exp2) of
         ((Exp'Error typ'1), (Exp'Error typ'2)) => (Typ.aequiv (typ'1, typ'2))
       | ((Exp'Int int'1), (Exp'Int int'2)) => (Int.equal (int'1, int'2))
       | ((Exp'Plus (exp'1, exp'3)), (Exp'Plus (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'LEq (exp'1, exp'3)), (Exp'LEq (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'List (typ'1, list'5)), (Exp'List (typ'2, list'6))) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso
            (ListPair.allEq
               (fn (exp'3, exp'4) =>
                  ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
               (list'5, list'6)))
       | ((Exp'Append (exp'1, exp'3)), (Exp'Append (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Index (exp'1, exp'3)), (Exp'Index (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Len exp'1), (Exp'Len exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
       | ((Exp'Pair labeled'3), (Exp'Pair labeled'4)) =>
           (Labeled.equal
              (fn (exp'1, exp'2) =>
                 ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2)))
              (labeled'3, labeled'4))
       | ((Exp'Proj (exp'1, label'3)), (Exp'Proj (exp'2, label'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso (Label.equal (label'3, label'4)))
       | ( (Exp'Inj (labeled'3, label'5, exp'7))
         , (Exp'Inj (labeled'4, label'6, exp'8))
         ) =>
           ((Labeled.equal (fn (typ'1, typ'2) => (Typ.aequiv (typ'1, typ'2)))
               (labeled'3, labeled'4)) andalso (Label.equal (label'5, label'6))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'7, exp'8)))
       | ( (Exp'Case (typ'1, exp'3, labeled'7))
         , (Exp'Case (typ'2, exp'4, labeled'8))
         ) =>
           ((Typ.aequiv (typ'1, typ'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4))
            andalso
            (Labeled.equal
               (fn ((_, exp'5), (_, exp'6)) =>
                  (true andalso ((Abt.aequiv exp_oper_aequiv) (exp'5, exp'6))))
               (labeled'7, labeled'8)))
       | ((Exp'Lam ((_, typ'1), exp'3)), (Exp'Lam ((_, typ'2), exp'4))) =>
           ((true andalso (Typ.aequiv (typ'1, typ'2)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Ap (exp'1, exp'3)), (Exp'Ap (exp'2, exp'4))) =>
           (((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Fold ((_, typ'1), exp'3)), (Exp'Fold ((_, typ'2), exp'4))) =>
           ((true andalso (Typ.aequiv (typ'1, typ'2)))
            andalso ((Abt.aequiv exp_oper_aequiv) (exp'3, exp'4)))
       | ((Exp'Unfold exp'1), (Exp'Unfold exp'2)) =>
           ((Abt.aequiv exp_oper_aequiv) (exp'1, exp'2))
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
    | Error of Typ.t
    | Int of Int.t
    | Plus of exp * exp
    | LEq of exp * exp
    | List of Typ.t * exp list
    | Append of exp * exp
    | Index of exp * exp
    | Len of exp
    | Pair of exp Labeled.t
    | Proj of exp * Label.t
    | Inj of Typ.t Labeled.t * Label.t * exp
    | Case of Typ.t * exp * (expVar * exp) Labeled.t
    | Lam of (expVar * Typ.t) * exp
    | Ap of exp * exp
    | Fold of (Typ.typVar * Typ.t) * exp
    | Unfold of exp

    fun view_in vars exp =
      (case exp of
         (Var x) => (Abt.Var x)
       | (Error typ'1) =>
           (Abt.Oper (ExpOps.Exp'Error ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                   typ'1 vars)
              , []
              ))))
       | (Int int'2) =>
           (Abt.Oper (ExpOps.Exp'Int ((fn (x, _) => x) (int'2, []))))
       | (Plus (exp'3, exp'4)) =>
           (Abt.Oper (ExpOps.Exp'Plus ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'3 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'4 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (LEq (exp'5, exp'6)) =>
           (Abt.Oper (ExpOps.Exp'LEq ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'5 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'6 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (List (typ'7, list'9)) =>
           (Abt.Oper (ExpOps.Exp'List ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'7 vars)
                  , []
                  )
                val (t1, vars'1) =
                  let
                    val (ts, vars') = (ListPair.unzip
                      (List.map
                         (fn exp'8 =>
                            ( (List.foldl
                                 (fn (x, acc) =>
                                    (Abt.into ExpOps.exp_oper_bind
                                       (Abt.Binding (x, acc)))) exp'8 vars)
                            , []
                            )) list'9))
                  in
                    (ts, (List.concat vars'))
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Append (exp'10, exp'11)) =>
           (Abt.Oper (ExpOps.Exp'Append ((fn (x, _) => x)
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
       | (Index (exp'12, exp'13)) =>
           (Abt.Oper (ExpOps.Exp'Index ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'12 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'13 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Len exp'14) =>
           (Abt.Oper (ExpOps.Exp'Len ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'14 vars)
              , []
              ))))
       | (Pair labeled'16) =>
           (Abt.Oper (ExpOps.Exp'Pair ((fn (x, _) => x)
              (Labeled.iter
                 (fn (exp'15, vars') =>
                    let
                      val (t, vars'') =
                        ( (List.foldl
                             (fn (x, acc) =>
                                (Abt.into ExpOps.exp_oper_bind
                                   (Abt.Binding (x, acc)))) exp'15 vars)
                        , []
                        )
                    in
                      (t, (vars'' @ vars'))
                    end) (labeled'16, [])))))
       | (Proj (exp'17, label'18)) =>
           (Abt.Oper (ExpOps.Exp'Proj ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'17 vars)
                  , []
                  )
                val (t1, vars'1) = (label'18, [])
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Inj (labeled'20, label'21, exp'22)) =>
           (Abt.Oper (ExpOps.Exp'Inj ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  (Labeled.iter
                     (fn (typ'19, vars') =>
                        let
                          val (t, vars'') =
                            ( (List.foldl
                                 (fn (x, acc) =>
                                    (Abt.into TypOps.typ_oper_bind
                                       (Abt.Binding (x, acc)))) typ'19 vars)
                            , []
                            )
                        in
                          (t, (vars'' @ vars'))
                        end) (labeled'20, []))
                val (t1, vars'1) = (label'21, [])
                val (t2, vars'2) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'22 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Case (typ'23, exp'24, labeled'27)) =>
           (Abt.Oper (ExpOps.Exp'Case ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into TypOps.typ_oper_bind (Abt.Binding (x, acc))))
                       typ'23 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'24 vars)
                  , []
                  )
                val (t2, vars'2) =
                  (Labeled.iter
                     (fn ((exp'25, exp'26), vars') =>
                        let
                          val (t, vars'') =
                            let
                              val (t, vars') =
                                let val var = exp'25
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
                        in
                          (t, (vars'' @ vars'))
                        end) (labeled'27, []))
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end)))
       | (Lam ((exp'28, typ'29), exp'30)) =>
           (Abt.Oper (ExpOps.Exp'Lam ((fn (x, _) => x)
              let
                val (t, vars') =
                  let
                    val (t0, vars'0) = let val var = exp'28
                                       in ((Temp.toUserString var), [var])
                                       end
                    val (t1, vars'1) =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'29 vars)
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
                       exp'30 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end)))
       | (Ap (exp'31, exp'32)) =>
           (Abt.Oper (ExpOps.Exp'Ap ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'31 vars)
                  , []
                  )
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'32 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Fold ((typ'33, typ'34), exp'35)) =>
           (Abt.Oper (ExpOps.Exp'Fold ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t, vars') = let val var = typ'33
                                     in ((Temp.toUserString var), [var])
                                     end
                    val vars = (vars' @ vars)
                    val (t', vars') =
                      ( (List.foldl
                           (fn (x, acc) =>
                              (Abt.into TypOps.typ_oper_bind
                                 (Abt.Binding (x, acc)))) typ'34 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
                  end
                val (t1, vars'1) =
                  ( (List.foldl
                       (fn (x, acc) =>
                          (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                       exp'35 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end)))
       | (Unfold exp'36) =>
           (Abt.Oper (ExpOps.Exp'Unfold ((fn (x, _) => x)
              ( (List.foldl
                   (fn (x, acc) =>
                      (Abt.into ExpOps.exp_oper_bind (Abt.Binding (x, acc))))
                   exp'36 vars)
              , []
              )))))

    fun oper_view_out vars exp =
      (case exp of
         (ExpOps.Exp'Error typ'1) =>
           (Error ((fn (x, _) => x)
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
       | (ExpOps.Exp'Int int'2) => (Int ((fn (x, _) => x) (int'2, [])))
       | (ExpOps.Exp'Plus (exp'3, exp'4)) =>
           (Plus ((fn (x, _) => x)
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
                          end) exp'3 vars)
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
                          end) exp'4 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'LEq (exp'5, exp'6)) =>
           (LEq ((fn (x, _) => x)
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
                          end) exp'5 vars)
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
                          end) exp'6 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'List (typ'7, list'9)) =>
           (List ((fn (x, _) => x)
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
                  let
                    val (ts, vars') = (ListPair.unzip
                      (List.map
                         (fn exp'8 =>
                            ( (List.foldr
                                 (fn (x, acc) =>
                                    let
                                      val (Abt.Binding (_, acc')) =
                                        (Abt.out ExpOps.exp_oper_unbind
                                           (Abt.unbind ExpOps.exp_oper_unbind x
                                              ~1 acc))
                                    in
                                      acc'
                                    end) exp'8 vars)
                            , []
                            )) list'9))
                  in
                    (ts, (List.concat vars'))
                  end
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Append (exp'10, exp'11)) =>
           (Append ((fn (x, _) => x)
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
       | (ExpOps.Exp'Index (exp'12, exp'13)) =>
           (Index ((fn (x, _) => x)
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
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'13 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Len exp'14) =>
           (Len ((fn (x, _) => x)
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
              )))
       | (ExpOps.Exp'Pair labeled'16) =>
           (Pair ((fn (x, _) => x)
              (Labeled.iter
                 (fn (exp'15, vars') =>
                    let
                      val (t, vars'') =
                        ( (List.foldr
                             (fn (x, acc) =>
                                let
                                  val (Abt.Binding (_, acc')) =
                                    (Abt.out ExpOps.exp_oper_unbind
                                       (Abt.unbind ExpOps.exp_oper_unbind x ~1
                                          acc))
                                in
                                  acc'
                                end) exp'15 vars)
                        , []
                        )
                    in
                      (t, (vars'' @ vars'))
                    end) (labeled'16, []))))
       | (ExpOps.Exp'Proj (exp'17, label'18)) =>
           (Proj ((fn (x, _) => x)
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
                          end) exp'17 vars)
                  , []
                  )
                val (t1, vars'1) = (label'18, [])
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Inj (labeled'20, label'21, exp'22)) =>
           (Inj ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  (Labeled.iter
                     (fn (typ'19, vars') =>
                        let
                          val (t, vars'') =
                            ( (List.foldr
                                 (fn (x, acc) =>
                                    let
                                      val (Abt.Binding (_, acc')) =
                                        (Abt.out TypOps.typ_oper_unbind
                                           (Abt.unbind TypOps.typ_oper_unbind x
                                              ~1 acc))
                                    in
                                      acc'
                                    end) typ'19 vars)
                            , []
                            )
                        in
                          (t, (vars'' @ vars'))
                        end) (labeled'20, []))
                val (t1, vars'1) = (label'21, [])
                val (t2, vars'2) =
                  ( (List.foldr
                       (fn (x, acc) =>
                          let
                            val (Abt.Binding (_, acc')) =
                              (Abt.out ExpOps.exp_oper_unbind
                                 (Abt.unbind ExpOps.exp_oper_unbind x ~1 acc))
                          in
                            acc'
                          end) exp'22 vars)
                  , []
                  )
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ExpOps.Exp'Case (typ'23, exp'24, labeled'27)) =>
           (Case ((fn (x, _) => x)
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
                          end) typ'23 vars)
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
                          end) exp'24 vars)
                  , []
                  )
                val (t2, vars'2) =
                  (Labeled.iter
                     (fn ((exp'25, exp'26), vars') =>
                        let
                          val (t, vars'') =
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
                                               (Abt.unbind
                                                  ExpOps.exp_oper_unbind x ~1
                                                  acc))
                                        in
                                          acc'
                                        end) exp'26 vars)
                                , []
                                )
                            in
                              ((t, t'), vars')
                            end
                        in
                          (t, (vars'' @ vars'))
                        end) (labeled'27, []))
              in
                ((t0, t1, t2), (vars'0 @ vars'1 @ vars'2))
              end))
       | (ExpOps.Exp'Lam ((exp'28, typ'29), exp'30)) =>
           (Lam ((fn (x, _) => x)
              let
                val (t, vars') =
                  let
                    val (t0, vars'0) = let val x = (Temp.new exp'28)
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
                              end) typ'29 vars)
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
                          end) exp'30 vars)
                  , []
                  )
              in
                ((t, t'), vars')
              end))
       | (ExpOps.Exp'Ap (exp'31, exp'32)) =>
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
                          end) exp'31 vars)
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
                          end) exp'32 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Fold ((typ'33, typ'34), exp'35)) =>
           (Fold ((fn (x, _) => x)
              let
                val (t0, vars'0) =
                  let
                    val (t, vars') = let val x = (Temp.new typ'33)
                                     in (x, [x])
                                     end
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
                              end) typ'34 vars)
                      , []
                      )
                  in
                    ((t, t'), vars')
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
                          end) exp'35 vars)
                  , []
                  )
              in
                ((t0, t1), (vars'0 @ vars'1))
              end))
       | (ExpOps.Exp'Unfold exp'36) =>
           (Unfold ((fn (x, _) => x)
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
              ))))

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
    val Error' = (into o Error)
    val Int' = (into o Int)
    val Plus' = (into o Plus)
    val LEq' = (into o LEq)
    val List' = (into o List)
    val Append' = (into o Append)
    val Index' = (into o Index)
    val Len' = (into o Len)
    val Pair' = (into o Pair)
    val Proj' = (into o Proj)
    val Inj' = (into o Inj)
    val Case' = (into o Case)
    val Lam' = (into o Lam)
    val Ap' = (into o Ap)
    val Fold' = (into o Fold)
    val Unfold' = (into o Unfold)
  end

  fun exp_toString exp =
    (case (Exp.out exp) of
       (Exp.Var x) => (Exp.Var.toString x)
     | (Exp.Error typ'1) => ("(Error " ^ (Typ.toString typ'1) ^ ")")
     | (Exp.Int int'2) => ("(Int " ^ (Int.toString int'2) ^ ")")
     | (Exp.Plus (exp'3, exp'4)) =>
         ("(Plus "
          ^ ("(" ^ (exp_toString exp'3) ^ ", " ^ (exp_toString exp'4) ^ ")")
          ^ ")")
     | (Exp.LEq (exp'5, exp'6)) =>
         ("(LEq "
          ^ ("(" ^ (exp_toString exp'5) ^ ", " ^ (exp_toString exp'6) ^ ")")
          ^ ")")
     | (Exp.List (typ'7, list'9)) =>
         ("(List "
          ^
          ("(" (* ^ (Typ.toString typ'7) ^ ", " *)
           ^
           ("["
            ^
            String.concatWith ", "
              (List.map (fn exp'8 => (exp_toString exp'8)) list'9) ^ "]") ^ ")")
          ^ ")")
     | (Exp.Append (exp'10, exp'11)) =>
         ("(Append "
          ^ ("(" ^ (exp_toString exp'10) ^ ", " ^ (exp_toString exp'11) ^ ")")
          ^ ")")
     | (Exp.Index (exp'12, exp'13)) =>
         ("(Index "
          ^ ("(" ^ (exp_toString exp'12) ^ ", " ^ (exp_toString exp'13) ^ ")")
          ^ ")")
     | (Exp.Len exp'14) => ("(Len " ^ (exp_toString exp'14) ^ ")")
     | (Exp.Pair labeled'16) =>
         ("(Pair "
          ^ (Labeled.toString (fn exp'15 => (exp_toString exp'15)) labeled'16)
          ^ ")")
     | (Exp.Proj (exp'17, label'18)) =>
         ("(Proj "
          ^
          ("(" ^ (exp_toString exp'17) ^ ", " ^ (Label.toString label'18) ^ ")")
          ^ ")")
     | (Exp.Inj (labeled'20, label'21, exp'22)) =>
         ("(Inj "
          ^
          ("(" (* ^ (Labeled.toString (fn typ'19 =>
               (Typ.toString typ'19)
               ) labeled'20) ^ ", " *) ^ (Label.toString label'21) ^ ", "
           ^ (exp_toString exp'22) ^ ")") ^ ")")
     | (Exp.Case (typ'23, exp'24, labeled'27)) =>
         ("(Case "
          ^
          ("(" (* ^ (Typ.toString typ'23) ^ ", " *) ^ (exp_toString exp'24)
           ^ ", "
           ^
           (Labeled.toString
              (fn (exp'25, exp'26) =>
                 ("(" ^ (Exp.Var.toString exp'25) ^ " . "
                  ^ (exp_toString exp'26) ^ ")")) labeled'27) ^ ")") ^ ")")
     | (Exp.Lam ((exp'28, typ'29), exp'30)) =>
         ("(Lam "
          ^
          ("("
           ^
           ("(" ^ (Exp.Var.toString exp'28) ^ ", " ^ (Typ.toString typ'29) ^ ")")
           ^ " . " ^ (exp_toString exp'30) ^ ")") ^ ")")
     | (Exp.Ap (exp'31, exp'32)) =>
         ("(Ap "
          ^ ("(" ^ (exp_toString exp'31) ^ ", " ^ (exp_toString exp'32) ^ ")")
          ^ ")")
     | (Exp.Fold ((typ'33, typ'34), exp'35)) =>
         ("(Fold "
          ^
          ("(" (* ^ ("(" ^ (Typ.Var.toString typ'33) ^ " . " ^ (Typ.toString typ'34) ^ ")") ^ ", " *)
           ^ (exp_toString exp'35) ^ ")") ^ ")")
     | (Exp.Unfold exp'36) => ("(Unfold " ^ (exp_toString exp'36) ^ ")"))

  fun exp_substTyp t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (Exp.Var' y)
     | (Exp.Error typ'1) => (Exp.Error' (Typ.subst t x typ'1))
     | (Exp.Int int'2) => (Exp.Int' int'2)
     | (Exp.Plus (exp'3, exp'4)) =>
         (Exp.Plus' ((exp_substTyp t x exp'3), (exp_substTyp t x exp'4)))
     | (Exp.LEq (exp'5, exp'6)) =>
         (Exp.LEq' ((exp_substTyp t x exp'5), (exp_substTyp t x exp'6)))
     | (Exp.List (typ'7, list'9)) =>
         (Exp.List'
            ( (Typ.subst t x typ'7)
            , (List.map (fn exp'8 => (exp_substTyp t x exp'8)) list'9)
            ))
     | (Exp.Append (exp'10, exp'11)) =>
         (Exp.Append' ((exp_substTyp t x exp'10), (exp_substTyp t x exp'11)))
     | (Exp.Index (exp'12, exp'13)) =>
         (Exp.Index' ((exp_substTyp t x exp'12), (exp_substTyp t x exp'13)))
     | (Exp.Len exp'14) => (Exp.Len' (exp_substTyp t x exp'14))
     | (Exp.Pair labeled'16) =>
         (Exp.Pair'
            (Labeled.map (fn exp'15 => (exp_substTyp t x exp'15)) labeled'16))
     | (Exp.Proj (exp'17, label'18)) =>
         (Exp.Proj' ((exp_substTyp t x exp'17), label'18))
     | (Exp.Inj (labeled'20, label'21, exp'22)) =>
         (Exp.Inj'
            ( (Labeled.map (fn typ'19 => (Typ.subst t x typ'19)) labeled'20)
            , label'21
            , (exp_substTyp t x exp'22)
            ))
     | (Exp.Case (typ'23, exp'24, labeled'27)) =>
         (Exp.Case'
            ( (Typ.subst t x typ'23)
            , (exp_substTyp t x exp'24)
            , (Labeled.map
                 (fn (exp'25, exp'26) => (exp'25, (exp_substTyp t x exp'26)))
                 labeled'27)
            ))
     | (Exp.Lam ((exp'28, typ'29), exp'30)) =>
         (Exp.Lam' ((exp'28, (Typ.subst t x typ'29)), (exp_substTyp t x exp'30)))
     | (Exp.Ap (exp'31, exp'32)) =>
         (Exp.Ap' ((exp_substTyp t x exp'31), (exp_substTyp t x exp'32)))
     | (Exp.Fold ((typ'33, typ'34), exp'35)) =>
         (Exp.Fold'
            ((typ'33, (Typ.subst t x typ'34)), (exp_substTyp t x exp'35)))
     | (Exp.Unfold exp'36) => (Exp.Unfold' (exp_substTyp t x exp'36)))
  and exp_subst t x exp =
    (case (Exp.out exp) of
       (Exp.Var y) => (if (Exp.Var.equal (x, y)) then t else (Exp.Var' y))
     | (Exp.Error typ'1) => (Exp.Error' typ'1)
     | (Exp.Int int'2) => (Exp.Int' int'2)
     | (Exp.Plus (exp'3, exp'4)) =>
         (Exp.Plus' ((exp_subst t x exp'3), (exp_subst t x exp'4)))
     | (Exp.LEq (exp'5, exp'6)) =>
         (Exp.LEq' ((exp_subst t x exp'5), (exp_subst t x exp'6)))
     | (Exp.List (typ'7, list'9)) =>
         (Exp.List'
            (typ'7, (List.map (fn exp'8 => (exp_subst t x exp'8)) list'9)))
     | (Exp.Append (exp'10, exp'11)) =>
         (Exp.Append' ((exp_subst t x exp'10), (exp_subst t x exp'11)))
     | (Exp.Index (exp'12, exp'13)) =>
         (Exp.Index' ((exp_subst t x exp'12), (exp_subst t x exp'13)))
     | (Exp.Len exp'14) => (Exp.Len' (exp_subst t x exp'14))
     | (Exp.Pair labeled'16) =>
         (Exp.Pair'
            (Labeled.map (fn exp'15 => (exp_subst t x exp'15)) labeled'16))
     | (Exp.Proj (exp'17, label'18)) =>
         (Exp.Proj' ((exp_subst t x exp'17), label'18))
     | (Exp.Inj (labeled'20, label'21, exp'22)) =>
         (Exp.Inj'
            ( (Labeled.map (fn typ'19 => typ'19) labeled'20)
            , label'21
            , (exp_subst t x exp'22)
            ))
     | (Exp.Case (typ'23, exp'24, labeled'27)) =>
         (Exp.Case'
            ( typ'23
            , (exp_subst t x exp'24)
            , (Labeled.map
                 (fn (exp'25, exp'26) => (exp'25, (exp_subst t x exp'26)))
                 labeled'27)
            ))
     | (Exp.Lam ((exp'28, typ'29), exp'30)) =>
         (Exp.Lam' ((exp'28, typ'29), (exp_subst t x exp'30)))
     | (Exp.Ap (exp'31, exp'32)) =>
         (Exp.Ap' ((exp_subst t x exp'31), (exp_subst t x exp'32)))
     | (Exp.Fold ((typ'33, typ'34), exp'35)) =>
         (Exp.Fold' ((typ'33, typ'34), (exp_subst t x exp'35)))
     | (Exp.Unfold exp'36) => (Exp.Unfold' (exp_subst t x exp'36)))

  structure Exp =
  struct
    open Exp
    val toString = exp_toString
    val aequiv = (Abt.aequiv ExpOps.exp_oper_aequiv)
    val substTyp = exp_substTyp
    val subst = exp_subst
  end
end

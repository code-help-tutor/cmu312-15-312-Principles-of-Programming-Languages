structure SynExtHelpers =
struct
  open PyCF FPC

  type class = PyCF.Class.t

  type label = FPC.Label.t
  type 'a labeled = 'a FPC.Labeled.t
  type typ = FPC.Typ.t
  type typVar = FPC.Typ.typVar
  type exp = FPC.Exp.t
  type expVar = FPC.Exp.expVar

  val bindTy = fn s => fn tau => let val t = Typ.Var.new s in (t, tau t) end
  val bindExp = fn s => fn e => let val x = Exp.Var.new s in (x, e x) end

  val fromList = fn l =>
    List.foldr (fn ((k, v), d) => Labeled.insert d k v) Labeled.empty l

  val Unit = Typ.Prod' Labeled.empty
  val Triv = Exp.Pair' Labeled.empty

  val BoolTys = fromList [("true", Unit), ("false", Unit)]
  val Bool = Typ.Sum' BoolTys
  val True = Exp.Inj' (BoolTys, "true", Triv)
  val False = Exp.Inj' (BoolTys, "false", Triv)
  val If = fn rho =>
    fn e =>
      fn (e1, e0) =>
        Exp.Case' (rho, e, fromList
          [("true", (Exp.Var.new "u", e1)), ("false", (Exp.Var.new "u", e0))])

  val classToSummand = fn t =>
    fn Class.Bool => ("bool", Bool)
     | Class.Int => ("int", Typ.Int')
     | Class.List => ("list", Typ.List' t)
     | Class.Fun => ("fun", Typ.Arrow' (t, t))

  val DynTys = fn t =>
    fromList
      (List.map (classToSummand t)
         [Class.Bool, Class.Int, Class.List, Class.Fun])
  val DynView = Typ.Sum' o DynTys
  val Dyn = Typ.Rec' (bindTy "d" (DynView o Typ.Var'))
  val DynMatch = fn rho =>
    fn e =>
      fn (ebool, eint, elist, efun) =>
        Exp.Case' (rho, Exp.Unfold' e, fromList
          [ ("bool", bindExp "b" ebool)
          , ("int", bindExp "i" eint)
          , ("list", bindExp "l" elist)
          , ("fun", bindExp "f" efun)
          ])

  val SelfView = fn tau => fn t => Typ.Arrow' (t, tau)
  val Self = fn tau =>
    Typ.Rec' (bindTy "self" (fn t => SelfView tau (Typ.Var' t)))
  val FoldSelf = fn tau =>
    fn (x, e) =>
      Exp.Fold' (bindTy "self" (fn t => SelfView tau (Typ.Var' t)), Exp.Lam'
        ((x, Self tau), e))
  val Unroll = fn tau =>
    fn e =>
      let
        val s = Exp.Var.new "s"
      in
        (* ensure eagerness *)
        Exp.Ap'
          ( Exp.Lam' ((s, Self tau), Exp.Ap'
              (Exp.Unfold' (Exp.Var' s), Exp.Var' s))
          , e
          )
      end
end

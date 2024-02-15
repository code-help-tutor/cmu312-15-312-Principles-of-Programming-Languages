structure ContextMA :> CONTEXT_MA =
  let
    structure TypSet = MA.Typ.Var.Set
    structure TypContext =
      SetContext (structure Set = TypSet)

    structure ExpDict = MA.Exp.Var.Dict
    structure ExpContext =
      DictContext
        (structure Dict = ExpDict
         type codomain = MA.Typ.t)

    structure RefDict = MA.Ref.Dict
    structure RefContext =
    DictContext
        (structure Dict = RefDict
         type codomain = MA.Typ.t)

    structure Context =
    ProductContext(
      structure Context1 = ProductContext
                               (structure Context1 = TypContext
                                structure Context2 = ExpContext)
      structure Context2 = RefContext
    )
  in
    struct
      open Context

      type typVar = MA.Typ.Var.t

      fun singletonTyp t = ((TypSet.singleton t, ExpDict.empty), RefDict.empty)
      fun insertTyp ((typVars, expVars), refSyms) t = ((TypSet.insert typVars t, expVars), refSyms)
      fun findTyp ((typVars, expVars), refSyms) t = TypSet.member typVars t

      type expVar = MA.Exp.Var.t
      type typ = MA.Typ.t

      fun singletonExp x typ = ((TypSet.empty, ExpDict.singleton x typ), RefDict.empty)
      fun insertExp ((typVars, expVars), refSyms) x typ = ((typVars, ExpDict.insert expVars x typ), refSyms)
      fun findExp ((typVars, expVars), refSyms) x = ExpDict.find expVars x


      type ref = MA.Ref.t

      fun singletonRef r typ = ((TypSet.empty, ExpDict.empty), RefDict.singleton r typ)
      fun insertRef ((typVars, expVars), refSyms) r typ = ((typVars, expVars), RefDict.insert refSyms r typ)
      fun findRef ((typVars, expVars), refSyms) r = RefDict.find refSyms r

    end
  end

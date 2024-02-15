structure ContextCA :> CONTEXT_CA =
let
  structure VarDict = CA.Exp.Var.Dict
  structure VarDictContext =
    DictContext (structure Dict = VarDict type codomain = CA.Typ.t)

  structure ChanDict = CA.Chan.Dict
  structure ChanDictContext =
    DictContext (structure Dict = ChanDict type codomain = CA.Typ.t)

  structure Context =
    ProductContext
      (structure Context1 = VarDictContext structure Context2 = ChanDictContext)
in
  struct
    open Context

    type expVar = CA.Exp.Var.t
    type chan = CA.Chan.t
    type typ = CA.Typ.t

    fun singletonExp x typ =
      (VarDict.singleton x typ, ChanDict.empty)
    fun insertExp (expVars, chans) x typ =
      (VarDict.insert expVars x typ, chans)
    fun findExp (expVars, chans) x = VarDict.find expVars x

    fun singletonChan x typ =
      (VarDict.empty, ChanDict.singleton x typ)
    fun insertChan (expVars, chans) x typ =
      (expVars, ChanDict.insert chans x typ)
    fun findChan (expVars, chans) x = ChanDict.find chans x
  end
end

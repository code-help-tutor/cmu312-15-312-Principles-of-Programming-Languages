structure ContextFPC :> CONTEXT_FPC =
let
  structure Set = FPC.Typ.Var.Set
  structure SetContext = SetContext (structure Set = Set)

  structure Dict = FPC.Exp.Var.Dict
  structure DictContext =
    DictContext (structure Dict = Dict type codomain = FPC.Typ.t)

  structure Context =
    ProductContext
      (structure Context1 = SetContext structure Context2 = DictContext)
in
  struct
    open Context

    type typVar = FPC.Typ.Var.t

    fun singletonTyp t = (Set.singleton t, Dict.empty)
    fun insertTyp (typVars, expVars) t =
      (Set.insert typVars t, expVars)
    fun findTyp (typVars, expVars) t = Set.member typVars t

    type expVar = FPC.Exp.Var.t
    type typ = FPC.Typ.t

    fun singletonExp x typ =
      (Set.empty, Dict.singleton x typ)
    fun insertExp (typVars, expVars) x typ =
      (typVars, Dict.insert expVars x typ)
    fun findExp (typVars, expVars) x = Dict.find expVars x
  end
end

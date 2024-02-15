structure ContextPCF :> CONTEXT_PCF =
let
  structure Dict = PCF.Exp.Var.Dict
  structure DictContext =
    DictContext (structure Dict = Dict type codomain = PCF.Typ.t)
in
  struct
    open DictContext

    type var = PCF.Exp.Var.t
    type typ = PCF.Typ.t

    val singleton = Dict.singleton
    val insert = Dict.insert

    exception ForgotToInsertVariableIntoContext

    fun lookup ctx x =
      case Dict.find ctx x of
        NONE => raise ForgotToInsertVariableIntoContext
      | SOME t => t
  end
end

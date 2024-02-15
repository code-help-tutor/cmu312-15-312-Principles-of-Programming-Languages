structure ContextPSF :> CONTEXT_PSF =
let
  structure Dict = PSF.Exp.Var.Dict
  structure DictContext =
    DictContext (structure Dict = Dict type codomain = PSF.Typ.t)
in
  struct
    open DictContext

    type var = PSF.Exp.Var.t
    type typ = PSF.Typ.t

    val singleton = Dict.singleton
    val insert = Dict.insert

    exception ForgotToInsertVariableIntoContext

    fun lookup ctx x =
      case Dict.find ctx x of
        NONE => raise ForgotToInsertVariableIntoContext
      | SOME t => t
  end
end

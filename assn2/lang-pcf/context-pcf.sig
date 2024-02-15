signature CONTEXT_PCF =
sig
  include CONTEXT

  type var = PCF.Exp.Var.t
  type typ = PCF.Typ.t

  val singleton: var -> typ -> context
  val insert: context -> var -> typ -> context

  val lookup: context -> var -> typ
end

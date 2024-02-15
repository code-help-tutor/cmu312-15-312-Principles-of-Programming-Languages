signature CONTEXT_PSF =
sig
  include CONTEXT

  type var = PSF.Exp.Var.t
  type typ = PSF.Typ.t

  val singleton: var -> typ -> context
  val insert: context -> var -> typ -> context

  val lookup: context -> var -> typ
end

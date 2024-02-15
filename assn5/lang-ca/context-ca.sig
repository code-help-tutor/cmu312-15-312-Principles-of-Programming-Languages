signature CONTEXT_CA =
sig
  include CONTEXT

  type expVar = CA.Exp.Var.t
  type chan = CA.Chan.t
  type typ = CA.Typ.t

  val singletonExp: expVar -> typ -> context
  val insertExp: context -> expVar -> typ -> context
  val findExp: context -> expVar -> typ option

  val singletonChan: chan -> typ -> context
  val insertChan: context -> chan -> typ -> context
  val findChan: context -> chan -> typ option
end

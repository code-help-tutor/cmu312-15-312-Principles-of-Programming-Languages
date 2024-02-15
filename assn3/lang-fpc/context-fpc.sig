signature CONTEXT_FPC =
sig
  include CONTEXT

  type typVar = FPC.Typ.Var.t

  val singletonTyp: typVar -> context
  val insertTyp: context -> typVar -> context
  val findTyp: context -> typVar -> bool

  type expVar = FPC.Exp.Var.t
  type typ = FPC.Typ.t

  val singletonExp: expVar -> typ -> context
  val insertExp: context -> expVar -> typ -> context
  val findExp: context -> expVar -> typ option
end

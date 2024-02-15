signature STATICS =
sig
  exception TypeError of string

  type context = KPCFv.Typ.t Context.dict

  val checkStack: KPCFv.Stack.t -> KPCFv.Typ.t -> unit
  and inferTypeValue: context -> KPCFv.Value.t -> KPCFv.Typ.t
  and checkTypeValue: context -> KPCFv.Value.t -> KPCFv.Typ.t -> unit
  and inferTypeExp: context -> KPCFv.Exp.t -> KPCFv.Typ.t
  and checkTypeExp: context -> KPCFv.Exp.t -> KPCFv.Typ.t -> unit

  val checkState: KPCFv.State.t -> unit
end

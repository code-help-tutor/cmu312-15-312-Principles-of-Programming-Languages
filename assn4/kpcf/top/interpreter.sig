signature INTERPRETER_KPCF =
sig
  val repl: unit -> unit

  val eval: KPCF.Exp.t -> KPCFv.Value.t
  val evalv: KPCFv.Exp.t -> KPCFv.Value.t

  val checkFile: string -> KPCFv.Typ.t option
  val evalFile: string -> KPCFv.Value.t option
end

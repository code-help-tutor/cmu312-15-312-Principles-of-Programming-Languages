signature DYNAMICS =
sig
  exception Malformed

  datatype transition = Step of KPCFv.State.t | Final of KPCFv.Value.t

  val progress: KPCFv.State.t -> transition
end

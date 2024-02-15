signature ELABORATOR =
sig
  val elaborateTyp: KPCF.Typ.t -> KPCFv.Typ.t
  and elaborateExp: KPCF.Exp.t -> KPCFv.Exp.t
end

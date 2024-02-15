structure VariableContext = SplayDict (structure Key = struct
                                         type t = MA.Exp.expVar
                                         val eq = MA.Exp.Var.equal
                                         val compare = MA.Exp.Var.compare
                                       end)

structure TypVariableContext = SplayDict (structure Key = struct
                                         type t = MA.Typ.typVar
                                         val eq = MA.Typ.Var.equal
                                         val compare = MA.Typ.Var.compare
                                       end)

structure SymbolContext = SplayDict (structure Key = struct
                                       type t = MA.Ref.t
                                       val eq = MA.Ref.equal
                                       val compare = MA.Ref.compare
                                     end)

structure SymbolSet = SplaySet (structure Elem = struct
                                  type t = MA.Ref.t
                                  val eq = MA.Ref.equal
                                  val compare = MA.Ref.compare
                                end)

structure VariableContext =
  SplayDict
    (structure Key =
     struct
       type t = CA.Exp.expVar
       val eq = CA.Exp.Var.equal
       val compare = CA.Exp.Var.compare
     end)

structure TypVariableContext =
  SplayDict
    (structure Key =
     struct
       type t = CA.Typ.typVar
       val eq = CA.Typ.Var.equal
       val compare = CA.Typ.Var.compare
     end)

structure SymbolContext =
  SplayDict
    (structure Key =
     struct
       type t = CA.Chan.t
       val eq = CA.Chan.equal
       val compare = CA.Chan.compare
     end)

structure SymbolSet =
  SplaySet
    (structure Elem =
     struct
       type t = CA.Chan.t
       val eq = CA.Chan.equal
       val compare = CA.Chan.compare
     end)

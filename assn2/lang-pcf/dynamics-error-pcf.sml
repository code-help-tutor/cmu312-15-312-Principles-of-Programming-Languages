structure DynamicsErrorPCF =
let structure Exp = PCF.Exp
in
  struct
    datatype t = Var of Exp.Var.t | Elim of string * Exp.t

    fun toString (error: t) : string =
      String.concat
        (case error of
           Var x =>
             ["cannot make progress on free variable ", Exp.Var.toString x]
         | Elim (elim, exp) =>
             [ "attempted to use elim form "
             , elim
             , ", but encountered non-canonical value "
             , Exp.toString exp
             ])
  end
end

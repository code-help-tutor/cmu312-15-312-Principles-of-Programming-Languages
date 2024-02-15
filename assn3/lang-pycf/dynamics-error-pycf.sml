structure DynamicsErrorPyCF =
let structure Object = PyCF.Object
in
  struct
    datatype t = Var of Object.Var.t | Elim of string * Object.t

    fun toString (error: t) : string =
      String.concat
        (case error of
           Var x =>
             ["cannot make progress on free variable ", Object.Var.toString x]
         | Elim (elim, exp) =>
             [ "attempted to use elim form "
             , elim
             , ", but encountered non-canonical value "
             , Object.toString exp
             ])
  end
end

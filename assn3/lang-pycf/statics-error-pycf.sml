structure StaticsErrorPyCF =
let structure Object = PyCF.Object
in
  struct
    datatype t = Var of Object.Var.t

    fun toString (error: t) : string =
      String.concat
        (case error of Var x => ["unbound variable ", Object.Var.toString x])
  end
end

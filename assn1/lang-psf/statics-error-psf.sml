structure StaticsErrorPSF =
let structure Typ = PSF.Typ and Exp = PSF.Exp
in
  struct
    datatype t = Elim of string * Exp.t * Typ.t | Check of Exp.t * Typ.t * Typ.t

    fun toString (error: t) : string =
      String.concat
        (case error of
           Elim (elim, exp, typ) =>
             [ "attempted to use elim form "
             , elim
             , " on "
             , Exp.toString exp
             , " : "
             , Typ.toString typ
             ]
         | Check (exp, typ, typ') =>
             [ "expected "
             , Exp.toString exp
             , " : "
             , Typ.toString typ
             , ", got "
             , Typ.toString typ'
             ])
  end
end

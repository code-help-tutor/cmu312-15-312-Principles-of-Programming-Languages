structure StaticsErrorFPC =
let structure Typ = FPC.Typ and Exp = FPC.Exp
in
  struct
    datatype t =
      TypVar of Typ.Var.t
    | ExpVar of Exp.Var.t
    | MissingLabel of string * FPC.Label.t
    | MisalignedLabels
    | Elim of string * Exp.t * Typ.t
    | Check of Exp.t * Typ.t * Typ.t

    fun toString (error: t) : string =
      String.concat
        (case error of
           TypVar t => ["unbound type variable ", Typ.Var.toString t]
         | ExpVar x => ["unbound expression variable ", Exp.Var.toString x]
         | MissingLabel (form, label) =>
             ["missing label ", FPC.Label.toString label, " in ", form]
         | MisalignedLabels => ["incorrect number of case arms"]
         | Elim (elim, exp, typ) =>
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

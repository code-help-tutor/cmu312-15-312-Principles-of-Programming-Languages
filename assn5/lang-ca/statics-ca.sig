signature STATICS_CA =
  sig
    structure Exp :
      STATICS
        where type Typ.t  = CA.Typ.t
          and type Term.t = CA.Exp.t
          and Context = ContextCA

    structure Cmd :
      STATICS
        where type Typ.t  = CA.Typ.t
          and type Term.t = CA.Cmd.t
          and Context = ContextCA
  end

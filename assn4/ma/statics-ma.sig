signature STATICS_MA =
  sig
    structure Exp :
      STATICS
        where type Typ.t  = MA.Typ.t
          and type Term.t = MA.Exp.t
          and Context = ContextMA

    structure Cmd :
      STATICS
        where type Typ.t  = MA.Typ.t
          and type Term.t = MA.Cmd.t
          and Context = ContextMA

    exception TypeError of StringError.t
  end

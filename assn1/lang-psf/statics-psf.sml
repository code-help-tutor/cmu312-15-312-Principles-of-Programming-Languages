structure StaticsPSF :>
  STATICS
    where type Typ.t  = PSF.Typ.t
      and type Term.t = PSF.Exp.t
      and Context = ContextPSF
  =
  let
    structure Typ = PSF.Typ
    and       Exp = PSF.Exp
  in
    struct
      structure Typ  = Typ
      and       Term = Exp

      structure Context = ContextPSF

      structure Error = StaticsErrorPSF
      exception TypeError of Error.t

      fun invalidElim (elim : string) (exp : Exp.t) (typ : Typ.t) : 'a =
        raise TypeError (Error.Elim (elim, exp, typ))

      fun inferType (ctx : Context.t) (exp : Exp.t) : Typ.t =
        raise Fail "TODO"

      and checkType (ctx : Context.t) (exp : Exp.t) (typ : Typ.t) : unit =
        let
          val typ' = inferType ctx exp
        in
          if Typ.aequiv (typ, typ')
            then ()
            else raise TypeError (Error.Check (exp, typ, typ'))
        end
    end
  end

structure StaticsPCF :>
  STATICS
    where type Typ.t  = PCF.Typ.t
      and type Term.t = PCF.Exp.t
      and Context = ContextPCF
  =
  let
    structure Typ = PCF.Typ
    and       Exp = PCF.Exp
  in
    struct
      structure Typ  = Typ
      and       Term = Exp

      structure Context = ContextPCF

      structure Error = StaticsErrorPCF
      exception TypeError of Error.t

      fun invalidElim (elim : string) (exp : Exp.t) (typ : Typ.t) : 'a =
        raise TypeError (Error.Elim (elim, exp, typ))

      fun inferType (ctx : Context.t) (exp : Exp.t) : Typ.t =
        case Exp.out exp of
          Exp.Var x => Context.lookup ctx x
        | Exp.Zero => Typ.Nat
        | Exp.Succ e =>
            ( checkType ctx e Typ.Nat
            ; Typ.Nat
            )
        | Exp.Ifz (e, e0, (x, e1)) => (
            case inferType ctx e of
              Typ.Nat =>
                let
                  val tau = inferType ctx e0
                in
                  checkType (Context.insert ctx x Typ.Nat) e1 tau;
                  tau
                end
            | typ => invalidElim "Ifz" e typ
          )
        | Exp.Fun ((tau1, tau2), (f, (x, e))) =>
            ( checkType (Context.insert (Context.insert ctx f (Typ.Arrow (tau1, tau2))) x tau1) e tau2
            ; Typ.Arrow (tau1, tau2)
            )
        | Exp.Ap (e, e1) => (
            case inferType ctx e of
              Typ.Arrow (tau1, tau) =>
                ( checkType ctx e1 tau1
                ; tau
                )
            | typ => invalidElim "Ap" e typ
          )

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

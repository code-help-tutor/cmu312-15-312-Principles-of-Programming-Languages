structure StaticsFPC :>
  STATICS
    where type Typ.t  = FPC.Typ.t
      and type Term.t = FPC.Exp.t
      and Context = ContextFPC
  =
  let
    structure Labeled = FPC.Labeled
    structure Typ = FPC.Typ
    and       Exp = FPC.Exp
  in
    struct
      structure Typ  = Typ
      and       Term = Exp

      structure Context = ContextFPC

      structure Error = StaticsErrorFPC
      exception TypeError of Error.t

      val unit = Typ.Prod' Labeled.empty
      val bool =
        Typ.Sum'
          (List.foldr
            (fn ((k, v), d) => Labeled.insert d k v)
            Labeled.empty
            [("true", unit), ("false", unit)])

      fun invalidElim (elim : string) (exp : Exp.t) (typ : Typ.t) : 'a =
        raise TypeError (Error.Elim (elim, exp, typ))

      fun validateType (ctx : Context.t) (typ : Typ.t) : unit =
        case Typ.out typ of
          Typ.Var t => (
            case Context.findTyp ctx t of
              false => raise TypeError (Error.TypVar t)
            | true => ()
          )
        | Typ.Int => ()
        | Typ.List tau => validateType ctx tau
        | Typ.Prod taus => Labeled.app (fn (_, tau) => validateType ctx tau) taus
        | Typ.Sum taus => Labeled.app (fn (_, tau) => validateType ctx tau) taus
        | Typ.Arrow (tau1, tau2) => (validateType ctx tau1; validateType ctx tau2)
        | Typ.Rec (t, tau) => validateType (Context.insertTyp ctx t) tau

      fun inferType (ctx : Context.t) (exp : Exp.t) : Typ.t =
        case Exp.out exp of
          Exp.Var x => (
            case Context.findExp ctx x of
              NONE => raise TypeError (Error.ExpVar x)
            | SOME tau => tau
          )
        | Exp.Error rho => (validateType ctx rho; rho)
        | Exp.Int _ => Typ.Int'
        | Exp.Plus (e1, e2) => (
            case Typ.out (inferType ctx e1) of
              Typ.Int => (
                case Typ.out (inferType ctx e2) of
                  Typ.Int => Typ.Int'
                | typ => invalidElim "Plus" e2 (Typ.into typ)
              )
            | typ => invalidElim "Plus" e1 (Typ.into typ)
          )
        | Exp.LEq (e1, e2) => (
            case Typ.out (inferType ctx e1) of
              Typ.Int => (
                case Typ.out (inferType ctx e2) of
                  Typ.Int => bool
                | typ => invalidElim "LEq" e2 (Typ.into typ)
              )
            | typ => invalidElim "LEq" e1 (Typ.into typ)
          )
        | Exp.List (tau, l) => (
            validateType ctx tau;
            List.app (fn ei => checkType ctx ei tau) l;
            Typ.List' tau
          )
        | Exp.Append (e1, e2) => (
            case Typ.out (inferType ctx e1) of
              Typ.List tau1 => (
                case Typ.out (inferType ctx e2) of
                  Typ.List tau2 =>
                    if Typ.aequiv (tau1, tau2)
                      then Typ.List' tau1
                      else invalidElim "Append" e2 (Typ.List' tau2)
                | typ => invalidElim "Append" e2 (Typ.into typ)
              )
            | typ => invalidElim "Append" e1 (Typ.into typ)
          )
        | Exp.Index (e, e_index) => (
            case Typ.out (inferType ctx e) of
              Typ.List tau => (
                case Typ.out (inferType ctx e_index) of
                  Typ.Int => tau
                | typ => invalidElim "LEq" e_index (Typ.into typ)
              )
            | typ => invalidElim "Index" e (Typ.into typ)
          )
        | Exp.Len e => (
            case Typ.out (inferType ctx e) of
              Typ.List _ => Typ.Int'
            | typ => invalidElim "Len" e (Typ.into typ)
          )
        | Exp.Lam ((x, tau1), e) =>
            let
              val () = validateType ctx tau1
              val tau2 = inferType (Context.insertExp ctx x tau1) e
            in
              Typ.Arrow' (tau1, tau2)
            end
        | Exp.Ap (e, e1) => (
            case Typ.out (inferType ctx e) of
              Typ.Arrow (tau1, tau2) =>
                ( checkType ctx e1 tau1
                ; tau2
                )
            | typ => invalidElim "Ap" e (Typ.into typ)
          )
        | Exp.Pair es => Typ.Prod' (Labeled.map (inferType ctx) es)
        | Exp.Proj (e, l) => (
            case Typ.out (inferType ctx e) of
              Typ.Prod taus => (
                case Labeled.find taus l of
                  NONE => raise TypeError (Error.MissingLabel ("Proj", l))
                | SOME taul => taul
              )
            | typ => invalidElim "Proj" e (Typ.into typ)
          )
        | Exp.Inj (taus, l, e) => (
            Labeled.app (fn (_, tau) => validateType ctx tau) taus;
            case Labeled.find taus l of
              NONE => raise TypeError (Error.MissingLabel ("Inj", l))
            | SOME taul => checkType ctx e taul;
            Typ.Sum' taus
          )
        | Exp.Case (tau, e, es) => (
            validateType ctx tau;
            case Typ.out (inferType ctx e) of
              Typ.Sum taus =>
                if Labeled.size es = Labeled.size taus
                  then
                    (Labeled.app
                      (fn (l, (xl, el)) =>
                        case Labeled.find taus l of
                          NONE => raise TypeError (Error.MissingLabel ("Case", l))
                        | SOME taul =>
                            checkType
                              (Context.insertExp ctx xl taul)
                              el
                              tau
                      )
                      es;
                    tau)
                  else raise TypeError Error.MisalignedLabels
            | typ => invalidElim "Case" e (Typ.into typ)
          )
        | Exp.Fold ((t, tau), e) => (
            validateType (Context.insertTyp ctx t) tau;
            checkType ctx e (Typ.subst (Typ.Rec' (t, tau)) t tau);
            Typ.Rec' (t, tau)
          )
        | Exp.Unfold e => (
            case Typ.out (inferType ctx e) of
              Typ.Rec (t, tau) => Typ.subst (Typ.Rec' (t, tau)) t tau
            | typ => invalidElim "Unfold" e (Typ.into typ)
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

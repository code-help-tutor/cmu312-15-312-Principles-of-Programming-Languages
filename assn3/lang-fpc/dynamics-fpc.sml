structure DynamicsFPC :>
  DYNAMICS
    where State = StateFPC
      and type term = FPC.Exp.t
  =
  let
    structure Labeled = FPC.Labeled
    structure Typ = FPC.Typ
    and       Exp = FPC.Exp
  in
    struct
      structure State = StateFPC

      type term = Exp.t

      structure Error = DynamicsErrorFPC
      exception Malformed of Error.t

      val fromList = fn l =>
        List.foldr
          (fn ((k, v), d) => Labeled.insert d k v)
          Labeled.empty
          l

      val Unit = Typ.Prod' Labeled.empty
      val Triv = Exp.Pair' Labeled.empty

      val BoolTys = fromList [("true", Unit), ("false", Unit)]
      val True = Exp.Inj' (BoolTys, "true", Triv)
      val False = Exp.Inj' (BoolTys, "false", Triv)

      fun progress (exp : Exp.t) : Exp.t State.t =
        case Exp.out exp of
          Exp.Var x => raise Malformed (Error.Var x)
        | Exp.Error _ => State.Err
        | Exp.Int i => State.Val (Exp.Int' i)
        | Exp.Plus (e1, e2) => (
            case progress e1 of
              State.Step e1' => State.Step (Exp.Plus' (e1', e2))
            | State.Val v1 => (
                case progress e2 of
                  State.Step e2' => State.Step (Exp.Plus' (e1, e2'))
                | State.Val v2 => (
                    case Exp.out v1 of
                      Exp.Int i1 => (
                        case Exp.out v2 of
                          Exp.Int i2 => State.Step (Exp.Int' (i1 + i2))
                        | _ => raise Malformed (Error.Elim ("Plus", v2))
                      )
                    | _ => raise Malformed (Error.Elim ("Plus", v1))
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Exp.LEq (e1, e2) => (
            case progress e1 of
              State.Step e1' => State.Step (Exp.LEq' (e1', e2))
            | State.Val v1 => (
                case progress e2 of
                  State.Step e2' => State.Step (Exp.LEq' (e1, e2'))
                | State.Val v2 => (
                    case Exp.out v1 of
                      Exp.Int i1 => (
                        case Exp.out v2 of
                          Exp.Int i2 => State.Step (if i1 <= i2 then True else False)
                        | _ => raise Malformed (Error.Elim ("LEq", v2))
                      )
                    | _ => raise Malformed (Error.Elim ("LEq", v1))
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Exp.List (tau, es) =>
            let
              fun progressList es =
                case es of
                  nil     => State.Val nil
                | e :: es => (
                    case progress e of
                      State.Step e' => State.Step (e' :: es)
                    | State.Val v =>
                        State.map2
                          (Fn.curry op:: v, Fn.curry op:: v)
                          (progressList es)
                    | State.Err => State.Err
                  )
            in
              State.map2
                (Fn.curry Exp.List' tau, Fn.curry Exp.List' tau)
                (progressList es)
            end
        | Exp.Append (e1, e2) => (
            case progress e1 of
              State.Step e1' => State.Step (Exp.Append' (e1', e2))
            | State.Val v1 => (
                case progress e2 of
                  State.Step e2' => State.Step (Exp.Append' (e1, e2'))
                | State.Val v2 => (
                    case Exp.out v1 of
                      Exp.List (tau, l1) => (
                        case Exp.out v2 of
                          Exp.List (_, l2) => State.Step (Exp.List' (tau, l1 @ l2))
                        | _ => raise Malformed (Error.Elim ("Append", v2))
                      )
                    | _ => raise Malformed (Error.Elim ("Append", v1))
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Exp.Index (e, e_index) => (
            case progress e of
              State.Step e' => State.Step (Exp.Index' (e', e_index))
            | State.Val v => (
                case progress e_index of
                  State.Step e_index' => State.Step (Exp.Index' (e, e_index'))
                | State.Val v_index => (
                    case Exp.out v of
                      Exp.List (_, l) => (
                        case Exp.out v_index of
                          Exp.Int i =>
                            if 0 <= i andalso i < List.length l
                              then State.Step (List.nth (l, i))
                              else State.Err
                        | _ => raise Malformed (Error.Elim ("Index", v_index))
                      )
                    | _ => raise Malformed (Error.Elim ("Index", v))
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Exp.Len e => (
            case progress e of
              State.Step e' => State.Step (Exp.Len' e')
            | State.Val v => (
                case Exp.out v of
                  Exp.List (_, l) => State.Step (Exp.Int' (List.length l))
                | _ => raise Malformed (Error.Elim ("Len", v))
              )
            | State.Err => State.Err
          )
        | Exp.Pair es =>
            let
              val d' =
                Labeled.foldl
                  (fn (l, el, State.Step es) => State.Step (Labeled.insert es l el)
                    | (l, el, State.Val es ) => (
                        case progress el of
                          State.Step el' => State.Step (Labeled.insert es l el')
                        | State.Val vl => State.Val (Labeled.insert es l vl)
                        | State.Err => State.Err
                      )
                    | (l, el, State.Err) => State.Err
                  )
                  (State.Val Labeled.empty)
                  es
            in
              State.map2
                (Exp.Pair', Exp.Pair')
                d'
            end
        | Exp.Proj (e, l) => (
            case progress e of
              State.Step e' => State.Step (Exp.Proj' (e', l))
            | State.Val v => (
                case Exp.out v of
                  Exp.Pair vs => (
                    case Labeled.find vs l of
                      NONE => raise Malformed (Error.MissingLabel ("Proj", l))
                    | SOME vl => State.Step vl
                  )
                | _ => raise Malformed (Error.Elim ("Proj", v))
              )
            | State.Err     => State.Err
          )
        | Exp.Inj (taus, l, e) =>
            State.map2
              (fn e => Exp.Inj' (taus, l, e), fn e => Exp.Inj' (taus, l, e))
              (progress e)
        | Exp.Case (rho, e, es) => (
            case progress e of
              State.Step e' => State.Step (Exp.Case' (rho, e', es))
            | State.Val v => (
                case Exp.out v of
                  Exp.Inj (taus, l, e) => (
                    case Labeled.find es l of
                      NONE => raise Malformed (Error.MissingLabel ("Case", l))
                    | SOME (xl, el) => State.Step (Exp.subst e xl el)
                  )
                | _ => raise Malformed (Error.Elim ("Case", v))
              )
            | State.Err     => State.Err
          )
        | Exp.Lam ((x, tau), e) => State.Val (Exp.Lam' ((x, tau), e))
        | Exp.Ap (e, e1) => (
            case progress e of
              State.Step e' => State.Step (Exp.Ap' (e', e1))
            | State.Val v => (
                case progress e1 of
                  State.Step e1' => State.Step (Exp.Ap' (e, e1'))
                | State.Val v1 => (
                    case Exp.out v of
                      Exp.Lam ((x, _), e2) => State.Step (Exp.subst v1 x e2)
                    | _ => raise Malformed (Error.Elim ("Ap", v))
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Exp.Fold ((t, tau), e) =>
            State.map2
              (fn e => Exp.Fold' ((t, tau), e), fn e => Exp.Fold' ((t, tau), e))
              (progress e)
        | Exp.Unfold e => (
            case progress e of
              State.Step e' => State.Step (Exp.Unfold' e')
            | State.Val v => (
                case Exp.out v of
                  Exp.Fold ((t, tau), v) => State.Step v
                | _ => raise Malformed (Error.Elim ("Unfold", v))
              )
            | State.Err => State.Err
          )
    end
  end

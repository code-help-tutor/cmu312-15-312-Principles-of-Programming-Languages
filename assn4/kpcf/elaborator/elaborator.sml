structure Elaborator :> ELABORATOR =
struct
  structure T = KPCF.Typ and Tv = KPCFv.Typ

  structure E = KPCF.Exp and Ev = KPCFv.Exp and Vv = KPCFv.Value

  fun elaborateTyp (typ: T.t) : Tv.t =
    case T.out typ of
      T.Cont tau => Tv.Cont' (elaborateTyp tau)
    | T.Unit => Tv.Unit'
    | T.Prod (tau1, tau2) => Tv.Prod' (elaborateTyp tau1, elaborateTyp tau2)
    | T.Void => Tv.Void'
    | T.Sum (tau1, tau2) => Tv.Sum' (elaborateTyp tau1, elaborateTyp tau2)
    | T.Arrow (tau1, tau2) => Tv.Arrow' (elaborateTyp tau1, elaborateTyp tau2)
    | T.Nat => Tv.Nat'
    | T.A => Tv.A'
    | T.B => Tv.B'
    | T.C => Tv.C'
    | T.D => Tv.D'

  fun elaborateExp (exp: E.t) : Ev.t =
    let
      fun bind (e: E.t) (xstring: string) (k: Vv.Var.t -> Ev.t) : Ev.t =
        let val x = Vv.Var.new xstring
        in Ev.Bind' (Vv.Comp' (elaborateExp e), (x, k x))
        end
    in
      case E.out exp of
        E.Var x => Ev.Ret' (Vv.Var' x)
      | E.Let (e1, (x, e2)) =>
          Ev.Bind' (Vv.Comp' (elaborateExp e1), (x, elaborateExp e2))
      | E.Letcc (tau, (x, e)) =>
          Ev.Letcc' (elaborateTyp tau, (x, elaborateExp e))
      | E.Throw (tau, e, e1) =>
          bind e "throw_e_" (fn xe =>
            bind e1 "throw_e1_" (fn xe1 =>
              Ev.Throw' (elaborateTyp tau, Vv.Var' xe, Vv.Var' xe1)))
      | E.Unit => Ev.Ret' Vv.Unit'
      | E.Tuple (e1, e2) =>
          bind e1 "tuple_e1_" (fn xe1 =>
            bind e2 "tuple_e2_" (fn xe2 =>
              Ev.Ret' (Vv.Tuple' (Vv.Var' xe1, Vv.Var' xe2))))
      | E.Split (e, ((x1, x2), e')) =>
          bind e "split_e_" (fn xe =>
            Ev.Split' (Vv.Var' xe, ((x1, x2), elaborateExp e')))
      | E.Abort (tau, e) =>
          bind e "abort_e_" (fn xe => Ev.Abort' (elaborateTyp tau, Vv.Var' xe))
      | E.InjL ((tau1, tau2), e) =>
          bind e "injl_e_" (fn xe =>
            Ev.Ret' (Vv.InjL'
              ((elaborateTyp tau1, elaborateTyp tau2), Vv.Var' xe)))
      | E.InjR ((tau1, tau2), e) =>
          bind e "injr_e_" (fn xe =>
            Ev.Ret' (Vv.InjR'
              ((elaborateTyp tau1, elaborateTyp tau2), Vv.Var' xe)))
      | E.Case (e, (x1, e1), (x2, e2)) =>
          bind e "case_e_" (fn xe =>
            Ev.Case' (Vv.Var' xe, (x1, elaborateExp e1), (x2, elaborateExp e2)))
      | E.Fun ((tau1, tau2), ((f, x), e)) =>
          Ev.Ret' (Vv.Fun'
            ((elaborateTyp tau1, elaborateTyp tau2), ((f, x), elaborateExp e)))
      | E.Lam ((x, tau), e) =>
          Ev.Ret' (Vv.Lam' ((x, elaborateTyp tau), elaborateExp e))
      | E.Ap (e, e1) =>
          bind e "ap_e_" (fn xe =>
            bind e1 "ap_e1_" (fn xe1 => Ev.Ap' (Vv.Var' xe, Vv.Var' xe1)))
      | E.Zero => Ev.Ret' Vv.Zero'
      | E.Succ e => bind e "succ_e_" (fn xe => Ev.Ret' (Vv.Succ' (Vv.Var' xe)))
      | E.Ifz (e, e0, (x, e1)) =>
          bind e "ifz_e_" (fn xe =>
            Ev.Ifz' (Vv.Var' xe, elaborateExp e0, (x, elaborateExp e1)))
    end
end

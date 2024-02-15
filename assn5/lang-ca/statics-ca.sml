structure StaticsCA :> STATICS_CA =
struct
  structure Typ = CA.Typ
  structure Exp = CA.Exp
  structure Cmd = CA.Cmd

  structure Context = ContextCA

  structure Error = StringError
  exception TypeError of Error.t

  fun inferTypeExp (ctx : Context.t) (e : Exp.t) : Typ.t =
      case Exp.out e
        of  Exp.Var x => (
                case Context.findExp ctx x of
                  NONE => raise TypeError "unbound variable"
                | SOME tau => tau
              )
          | Exp.Fun ((tau1, tau2), ((f, x), e)) =>
              let
                val tau = Typ.Arr' (tau1, tau2)
              in
                checkTypeExp (Context.insertExp (Context.insertExp ctx f tau) x tau1) e tau2; tau
              end
          | Exp.Lam (t,(x,e)) => Typ.Arr' (t, inferTypeExp (Context.insertExp ctx x t) e)
          | Exp.App (e1, e2) => (
              case Typ.out (inferTypeExp ctx e1) of
                Typ.Arr (t1, t2) => (checkTypeExp ctx e2 t1; t2)
              | _ => raise TypeError "Non-function applied to argument."
            )
          | Exp.Triv => Typ.Unit'
          | Exp.Pair (e1, e2) => Typ.Star' (inferTypeExp ctx e1, inferTypeExp ctx e2)
          | Exp.Split (e1, ((x1, x2), e2)) => (
              case Typ.out (inferTypeExp ctx e1) of
                Typ.Star (tau1, tau2) => inferTypeExp (Context.insertExp (Context.insertExp ctx x1 tau1) x2 tau2) e2
              | _ => raise TypeError "Split principal argument type is not Star"
            )
          | Exp.Inl (t,e) => (
              case Typ.out t of
                Typ.Sum (t1, t2) => (checkTypeExp ctx e t1; t)
              | _ => raise TypeError "Not a sum type."
            )
          | Exp.Inr (t,e) => (
              case Typ.out t of
                Typ.Sum (t1, t2) => (checkTypeExp ctx e t2; t)
              | _ => raise TypeError "Not a sum type."
            )
          | Exp.Case (e, (x1,e1), (x2,e2)) => (
              case Typ.out (inferTypeExp ctx e) of
                Typ.Sum (tx1, tx2) =>
                  let
                    val t1 = inferTypeExp (Context.insertExp ctx x1 tx1) e1
                  in
                    checkTypeExp (Context.insertExp ctx x2 tx2) e2 t1; t1
                  end
              | _ => raise TypeError "Argument of case not a sum type."
            )
          | Exp.Fold (t', e) => (
              case Typ.out t' of
                Typ.Rec (t, tau) => (checkTypeExp ctx e (Typ.subst t' t tau); t')
              | _ => raise TypeError "Not a rec type."
            )
          | Exp.Unfold e => (
              case Typ.out (inferTypeExp ctx e) of
                Typ.Rec (t, tau) => Typ.subst (Typ.Rec' (t, tau)) t tau
              | _ => raise TypeError "Not a rec type."
            )
          | Exp.Abort (t, e) => (checkTypeExp ctx e Typ.Void'; t)
          | Exp.Num n => Typ.Nat'
          | Exp.Bool b => Typ.Bool'
          | Exp.If (cond, (e1, e2)) => (
              checkTypeExp ctx cond Typ.Bool';
              let val t1 = inferTypeExp ctx e1
              in checkTypeExp ctx e2 t1; t1 end
            )
          | Exp.Succ e => (checkTypeExp ctx e Typ.Nat'; Typ.Nat')
          | Exp.Ifz (cond, e1, (x,e2)) => (
              checkTypeExp ctx cond Typ.Nat';
              let val t1 = inferTypeExp ctx e1
              in checkTypeExp (Context.insertExp ctx x Typ.Nat') e2 t1; t1 end
            )
          | Exp.Let (e1, (x,e2)) =>
            let
              val t1 = inferTypeExp ctx e1
            in
              inferTypeExp (Context.insertExp ctx x t1) e2
            end
          | Exp.Cmd e => Typ.Cmd' (inferTypeCmd ctx e)
          | Exp.ChnRef a => (
                case Context.findChan ctx a of
                  NONE => raise TypeError "unbound channel"
                | SOME tau => Typ.Chan' tau
              )
          | Exp.String s => Typ.String'
          | Exp.Binop (b, e1, e2) => (
              case b of
                (Oper.Plus | Oper.Minus | Oper.Times | Oper.Div | Oper.Mod) => (
                checkTypeExp ctx e1 Typ.Nat';
                checkTypeExp ctx e2 Typ.Nat';
                Typ.Nat'
              )
              | (Oper.AndAnd | Oper.OrOr) => (
                checkTypeExp ctx e1 Typ.Bool';
                checkTypeExp ctx e2 Typ.Bool';
                Typ.Bool'
              )
              | (Oper.Lt | Oper.Lte | Oper.Gt | Oper.Gte) => (
                checkTypeExp ctx e1 Typ.Nat';
                checkTypeExp ctx e2 Typ.Nat';
                Typ.Bool'
              )
              | (Oper.Eq | Oper.Neq) => (
                case (Typ.out (inferTypeExp ctx e1), Typ.out (inferTypeExp ctx e2)) of
                  (Typ.Nat, Typ.Nat) => Typ.Bool'
                | (Typ.Bool, Typ.Bool) => Typ.Bool'
                | _ => raise TypeError "Illegal comparison."
              )
            )
          | Exp.Negate e => (checkTypeExp ctx e Typ.Bool'; Typ.Bool')

    and inferTypeCmd (ctx : Context.t) (cmd : Cmd.t) : Typ.t =
        case Cmd.out cmd
          of Cmd.Ret e => inferTypeExp ctx e
            | Cmd.Bind (e, (x, m)) => (
                case Typ.out (inferTypeExp ctx e) of
                  Typ.Cmd tm => inferTypeCmd (Context.insertExp ctx x tm) m
                | _ => raise TypeError "Not a command."
              )
            | Cmd.Spawn e => (
                case Typ.out (inferTypeExp ctx e) of
                  Typ.Cmd tm => Typ.Chan' tm
                | _ => raise TypeError "Not a command."
              )
            | Cmd.Emit (e1, e2) => (
                case Typ.out (inferTypeExp ctx e1) of
                  Typ.Chan t1' => (checkTypeExp ctx e2 t1'; Typ.Unit')
                | _ => raise TypeError "Not a Channel."
              )
            | Cmd.Sync e => (
                case Typ.out (inferTypeExp ctx e) of
                  Typ.Chan t' => t'
                | _ => raise TypeError "Not a channel."
              )
            | Cmd.NewChn (t, (a, m)) => inferTypeCmd (Context.insertChan ctx a t) m
            | Cmd.Print e => (
                case Typ.out (inferTypeExp ctx e) of
                  (Typ.String | Typ.Nat) => Typ.Unit'
                | _ => raise TypeError "Illegal print."
              )

    and checkTypeExp (ctx : Context.t) (exp : Exp.t) (typ : Typ.t) : unit =
      let
        val typ' = inferTypeExp ctx exp
      in
        if Typ.aequiv (typ, typ')
          then ()
          else raise TypeError ("Expected " ^ (Exp.toString exp) ^ " : "  ^ (Typ.toString typ) ^ ", got " ^ (Typ.toString typ'))
      end

    and checkTypeCmd (ctx : Context.t) (cmd : Cmd.t) (typ : Typ.t) : unit =
      let
        val typ' = inferTypeCmd ctx cmd
      in
        if Typ.aequiv (typ, typ')
          then ()
          else raise TypeError ("Expected " ^ (Cmd.toString cmd) ^ " : "  ^ (Typ.toString typ) ^ ", got " ^ (Typ.toString typ'))
      end

    structure Cmd =
      struct
        structure Typ  = Typ
        and       Term = Cmd

        structure Context = ContextCA

        structure Error = Error
        exception TypeError of Error.t

        val inferType = inferTypeCmd
        and checkType = checkTypeCmd
      end

    structure Exp =
      struct
        structure Typ  = Typ
        and       Term = Exp

        structure Context = ContextCA

        structure Error = Error
        exception TypeError of Error.t

        val inferType = inferTypeExp
        and checkType = checkTypeExp
      end
 end

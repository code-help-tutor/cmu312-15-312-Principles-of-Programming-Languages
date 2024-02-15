structure DynamicsCAExp :>
  DYNAMICS
    where State = StateCAExp
      and type term = CA.Exp.t
  =
  let
    structure Exp = CA.Exp
  in
    struct
      structure State = StateCAExp

      type term = Exp.t

      structure Error = StringError
      exception Malformed of Error.t
      exception RuntimeError = Fail

      fun progress (exp : Exp.t) : Exp.t State.t =
        case Exp.out exp of
          Exp.Num _ => State.Val exp
        | Exp.Succ e => (
            case progress e of
              State.Step e' => State.Step (Exp.Succ' e')
            | State.Val v => (
                case Exp.out v of
                  Exp.Num n => State.Step (Exp.Num' (n + 1))
                | _ => raise Malformed "Succ argument is not a number"
              )
          )
        | Exp.Ifz (e0, e1, (x, e2)) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Ifz' (e0', e1, (x, e2)))
            | State.Val v0 =>
              case Exp.out v0 of
                Exp.Num 0 => State.Step e1
              | Exp.Num n => State.Step (Exp.subst (Exp.Num' (n-1)) x e2)
              | _ => raise Malformed "Not a Number"
          )
        | Exp.Bool _ => State.Val exp
        | Exp.If (e0, (e1, e2)) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.If' (e0', (e1, e2)))
            | State.Val v0 =>
              case Exp.out v0 of
                Exp.Bool true => State.Step e1
              | Exp.Bool false => State.Step e2
              | _ => raise Malformed "Not a Boolean"
          )
        | Exp.Let (e0, (x, e1)) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Let' (e0', (x, e1)))
            | State.Val v0 => State.Step (Exp.subst v0 x e1)
          )
        | Exp.Fun _ => State.Val exp
        | Exp.Lam _ => State.Val exp
        | Exp.App (e0, e1) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.App' (e0', e1))
            | State.Val v0 =>
                case progress e1 of
                  State.Step e1' => State.Step (Exp.App' (v0, e1'))
                | State.Val v1 => (
                    case Exp.out v0 of
                      Exp.Fun ((t1, t2), ((f, x), e2)) => State.Step (Exp.subst v1 x (Exp.subst v0 f e2))
                    | Exp.Lam (t, (x, e0')) => State.Step (Exp.subst v1 x e0')
                    | _ => raise Malformed "Application on non-fun/lam."
                  )
          )
        | Exp.Abort (t, e0) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Abort' (t, e0'))
            | State.Val v0 => raise Malformed "Abort called on a value." (* this will never be raised *)
          )
        | Exp.Inl (t, e0) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Inl' (t, e0'))
            | State.Val v0 => State.Val exp
          )
        | Exp.Inr (t, e0) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Inr' (t, e0'))
            | State.Val v0 => State.Val exp
          )
        | Exp.Case (e0, (x1, e1), (x2, e2)) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Case' (e0', (x1, e1), (x2, e2)))
            | State.Val v0 => (
              case Exp.out v0 of
                Exp.Inl (t, e0') => State.Step (Exp.subst e0' x1 e1)
              | Exp.Inr (t, e0') => State.Step (Exp.subst e0' x2 e2)
              | _ => raise Malformed "Case on unknown term."
            )
          )
        | Exp.Triv => State.Val exp
        | Exp.Pair (e0, e1) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Pair' (e0', e1))
            | State.Val v0 =>
              case progress e1 of
                State.Step e1' => State.Step (Exp.Pair' (v0, e1'))
              | State.Val v1 => State.Val exp
          )
        | Exp.Split (e1, ((x1, x2), e2)) => (
            case progress e1 of
              State.Step e1' => State.Step (Exp.Split' (e1', ((x1, x2), e2)))
            | State.Val v1 => (
                case Exp.out v1 of
                  Exp.Pair (p1, p2) => State.Step (Exp.subst p2 x2 (Exp.subst p1 x1 e2))
                | _ => raise Malformed "Split principal argument not a pair"
              )
          )
        | Exp.Fold (t, e0) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Fold' (t, e0'))
            | State.Val v0 => State.Val exp
          )
        | Exp.Unfold e => (
            case progress e of
              State.Step e' => State.Step (Exp.Unfold' e')
            | State.Val v => (
                case Exp.out v of
                  Exp.Fold (_, e1) => State.Step e1
                | _ => raise Malformed "Unfold"
              )
          )
        | Exp.Cmd m => State.Val exp
        | Exp.ChnRef a => State.Val exp
        | Exp.String s => State.Val exp
        | Exp.Binop (b, e0, e1) => (
            case progress e0 of
              State.Step e0' => State.Step (Exp.Binop' (b, e0', e1))
            | State.Val v0 =>
              case progress e1 of
                State.Step e1' => State.Step (Exp.Binop' (b, v0, e1'))
              | State.Val v1 =>
                let
                  fun get_num (Exp.Num n) = n
                    | get_num _ = raise Malformed "expected binop argument to be Num"
                  fun get_bool (Exp.Bool b) = b
                    | get_bool _ = raise Malformed "expected binop argument to be Bool"
                  val (e0, e1) = (Exp.out v0, Exp.out v1)
                  fun num_binop binop =
                    State.Step (Exp.Num' (binop (get_num e0, get_num e1)))
                  fun bool_binop binop =
                    State.Step (Exp.Bool' (binop (get_bool e0, get_bool e1)))
                  fun cmp_binop binop =
                    State.Step (Exp.Bool' (binop (get_num e0, get_num e1)))
                in
                  case b of
                    Oper.Plus => num_binop op+
                  | Oper.Minus => num_binop op-
                  | Oper.Times => num_binop op*
                  | Oper.Div => num_binop (fn (x, y) => x div y handle Div => raise RuntimeError "div by zero")
                  | Oper.Mod => num_binop (fn (x, y) => x mod y handle Div => raise RuntimeError "mod by zero")
                  | Oper.AndAnd => bool_binop (fn (x, y) => x andalso y)
                  | Oper.OrOr => bool_binop (fn (x, y) => x orelse y)
                  | Oper.Lt => cmp_binop op<
                  | Oper.Gt => cmp_binop op>
                  | Oper.Lte => cmp_binop op<=
                  | Oper.Gte => cmp_binop op>=
                  | Oper.Eq => (
                    case (e0, e1) of
                      (Exp.Bool b0, Exp.Bool b1) =>
                      State.Step (Exp.Bool' (b0 = b1))
                    | (Exp.Num n0, Exp.Num n1) =>
                      State.Step (Exp.Bool' (n0 = n1))
                    | _ => raise Malformed "Illegal comparison"
                  )
                  | Oper.Neq => (
                    case (e0, e1) of
                      (Exp.Bool b0, Exp.Bool b1) =>
                      State.Step (Exp.Bool' (b0 <> b1))
                    | (Exp.Num n0, Exp.Num n1) =>
                      State.Step (Exp.Bool' (n0 <> n1))
                    | _ => raise Malformed "Illegal comparison"
                  )
                end
        )
        | Exp.Negate e => (
            case progress e of
              State.Step e' => State.Step (Exp.Negate' e')
            | State.Val v => (
                case Exp.out v of
                  Exp.Bool b => State.Step (Exp.Bool' (not b))
                | _ => raise Malformed "Negate"
              )
        )
        | Exp.Var _ => raise Malformed "Unknown expression"
    end
  end

functor ParseTerm (ULC : ULC) :> PARSE_TERM where ULC = ULC = struct
  structure ULC = ULC
  structure Term = ULC.Term

  datatype t =
    Var of string
  | App of t * t
  | Lam of string * t
  | Norm of t
  | Normed of Term.t

  structure Context = SplayDict (structure Key = StringOrdered)

  fun add_defs e syms =
    case e of
      Var s => (case Symbols.find syms s of NONE => e | SOME e => e)
    | App (e1, e2) => App (add_defs e1 syms, add_defs e2 syms)
    | Lam (s, e) => Lam (s, add_defs e (Symbols.remove syms s))
    | Norm e => Norm (add_defs e syms)
    | Normed _ => e

  fun to_term e norm =
    let
      fun to_term' ctx e =
        case e of
          Var s => Term.Var' (Context.lookup ctx s handle Context.Absent => Term.Var.new s)
        | App (e1, e2) => Term.Ap' (to_term' ctx e1, to_term' ctx e2)
        | Lam (s, e) =>
            let
              val x = Term.Var.new s
            in
              Term.Lam' (x, to_term' (Context.insert ctx s x) e)
            end
        | Norm e => norm (to_term' ctx e)
        | Normed e => e
    in
      to_term' Context.empty e
    end

  fun norm e norm_fn = Normed (to_term e norm_fn)
end

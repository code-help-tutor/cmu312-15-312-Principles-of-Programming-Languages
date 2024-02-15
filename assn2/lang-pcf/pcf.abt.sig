signature PCF =
sig

  structure Typ:
  sig
    datatype typ = Nat | Arrow of typ * typ

    type t = typ

    val aequiv: t * t -> bool

    val toString: t -> string
  end

  structure Exp:
  sig
    type expVar
    type exp
    type t = exp

    structure Var: TEMP where type t = expVar

    datatype view =
      Var of expVar
    | Zero
    | Succ of exp
    | Ifz of exp * exp * (expVar * exp)
    | Fun of (Typ.t * Typ.t) * (expVar * (expVar * exp))
    | Ap of exp * exp

    val Var': expVar -> exp
    val Zero': exp
    val Succ': exp -> exp
    val Ifz': exp * exp * (expVar * exp) -> exp
    val Fun': (Typ.t * Typ.t) * (expVar * (expVar * exp)) -> exp
    val Ap': exp * exp -> exp

    val into: view -> exp
    val out: exp -> view
    val aequiv: exp * exp -> bool
    val toString: exp -> string

    val subst: exp -> expVar -> exp -> exp
  end
end

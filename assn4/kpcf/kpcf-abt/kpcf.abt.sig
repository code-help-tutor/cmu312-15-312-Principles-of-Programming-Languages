signature KPCF =
sig

  structure Typ:
  sig
    type typ
    type t = typ

    datatype view =
      Cont of typ
    | Unit
    | Prod of typ * typ
    | Void
    | Sum of typ * typ
    | Arrow of typ * typ
    | Nat
    | A
    | B
    | C
    | D

    val Cont': typ -> typ
    val Unit': typ
    val Prod': typ * typ -> typ
    val Void': typ
    val Sum': typ * typ -> typ
    val Arrow': typ * typ -> typ
    val Nat': typ
    val A': typ
    val B': typ
    val C': typ
    val D': typ

    val into: view -> typ
    val out: typ -> view
    val aequiv: typ * typ -> bool
    val toString: typ -> string
  end

  structure Exp:
  sig
    type expVar = Variable.t
    type exp
    type t = exp

    structure Var: TEMP where type t = expVar

    datatype view =
      Var of expVar
    | Let of exp * (expVar * exp)
    | Letcc of Typ.t * (expVar * exp)
    | Throw of Typ.t * exp * exp
    | Unit
    | Tuple of exp * exp
    | Split of exp * ((expVar * expVar) * exp)
    | Abort of Typ.t * exp
    | InjL of (Typ.t * Typ.t) * exp
    | InjR of (Typ.t * Typ.t) * exp
    | Case of exp * (expVar * exp) * (expVar * exp)
    | Fun of (Typ.t * Typ.t) * ((expVar * expVar) * exp)
    | Lam of (expVar * Typ.t) * exp
    | Ap of exp * exp
    | Zero
    | Succ of exp
    | Ifz of exp * exp * (expVar * exp)

    val Var': expVar -> exp
    val Let': exp * (expVar * exp) -> exp
    val Letcc': Typ.t * (expVar * exp) -> exp
    val Throw': Typ.t * exp * exp -> exp
    val Unit': exp
    val Tuple': exp * exp -> exp
    val Split': exp * ((expVar * expVar) * exp) -> exp
    val Abort': Typ.t * exp -> exp
    val InjL': (Typ.t * Typ.t) * exp -> exp
    val InjR': (Typ.t * Typ.t) * exp -> exp
    val Case': exp * (expVar * exp) * (expVar * exp) -> exp
    val Fun': (Typ.t * Typ.t) * ((expVar * expVar) * exp) -> exp
    val Lam': (expVar * Typ.t) * exp -> exp
    val Ap': exp * exp -> exp
    val Zero': exp
    val Succ': exp -> exp
    val Ifz': exp * exp * (expVar * exp) -> exp

    val into: view -> exp
    val out: exp -> view
    val aequiv: exp * exp -> bool
    val toString: exp -> string

    val subst: exp -> expVar -> exp -> exp
  end
end

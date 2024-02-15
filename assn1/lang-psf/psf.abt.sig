signature PSF =
sig

  structure Typ:
  sig
    datatype typ =
      Arrow of typ * typ
    | Unit
    | Prod of typ * typ
    | Void
    | Sum of typ * typ
    | A
    | B
    | C

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
    | Lam of Typ.t * (expVar * exp)
    | Ap of exp * exp
    | Triv
    | Pair of exp * exp
    | PrL of exp
    | PrR of exp
    | Abort of Typ.t * exp
    | InL of (Typ.t * Typ.t) * exp
    | InR of (Typ.t * Typ.t) * exp
    | Case of Typ.t * exp * (expVar * exp) * (expVar * exp)

    val Var': expVar -> exp
    val Lam': Typ.t * (expVar * exp) -> exp
    val Ap': exp * exp -> exp
    val Triv': exp
    val Pair': exp * exp -> exp
    val PrL': exp -> exp
    val PrR': exp -> exp
    val Abort': Typ.t * exp -> exp
    val InL': (Typ.t * Typ.t) * exp -> exp
    val InR': (Typ.t * Typ.t) * exp -> exp
    val Case': Typ.t * exp * (expVar * exp) * (expVar * exp) -> exp

    val into: view -> exp
    val out: exp -> view
    val aequiv: exp * exp -> bool
    val toString: exp -> string

    val subst: exp -> expVar -> exp -> exp
  end
end

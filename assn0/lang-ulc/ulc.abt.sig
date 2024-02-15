signature ULC =
sig
  structure Term:
  sig
    type termVar
    type term
    type t = term

    structure Var: TEMP where type t = termVar

    datatype view = Var of termVar | Lam of termVar * term | Ap of term * term

    val Var': termVar -> term
    val Lam': termVar * term -> term
    val Ap': term * term -> term

    val into: view -> term
    val out: term -> view
    val aequiv: term * term -> bool
    val toString: term -> string

    val subst: term -> termVar -> term -> term
  end
end

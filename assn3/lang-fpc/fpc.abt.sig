signature FPC =
sig

  structure Int:
  sig
    type t
  end

  structure Label:
  sig
    type t
    val equal: t * t -> bool
    val toString: t -> string
  end

  structure Labeled:
  sig
    type 'a t
    val equal: ('a * 'a -> bool) -> 'a t * 'a t -> bool
    val toString: ('a -> string) -> 'a t -> string

    include DICT where type key = Label.t and type 'a dict = 'a t
  end

  structure Typ:
  sig
    type typVar
    type typ
    type t = typ

    structure Var: TEMP where type t = typVar

    datatype view =
      Var of typVar
    | Int
    | List of typ
    | Prod of typ Labeled.t
    | Sum of typ Labeled.t
    | Arrow of typ * typ
    | Rec of typVar * typ

    val Var': typVar -> typ
    val Int': typ
    val List': typ -> typ
    val Prod': typ Labeled.t -> typ
    val Sum': typ Labeled.t -> typ
    val Arrow': typ * typ -> typ
    val Rec': typVar * typ -> typ

    val into: view -> typ
    val out: typ -> view
    val aequiv: typ * typ -> bool
    val toString: typ -> string

    val subst: typ -> typVar -> typ -> typ
  end

  structure Exp:
  sig
    type expVar = Variable.t
    type exp
    type t = exp

    structure Var: TEMP where type t = expVar

    datatype view =
      Var of expVar
    | Error of Typ.t
    | Int of Int.t
    | Plus of exp * exp
    | LEq of exp * exp
    | List of Typ.t * exp list
    | Append of exp * exp
    | Index of exp * exp
    | Len of exp
    | Pair of exp Labeled.t
    | Proj of exp * Label.t
    | Inj of Typ.t Labeled.t * Label.t * exp
    | Case of Typ.t * exp * (expVar * exp) Labeled.t
    | Lam of (expVar * Typ.t) * exp
    | Ap of exp * exp
    | Fold of (Typ.typVar * Typ.t) * exp
    | Unfold of exp

    val Var': expVar -> exp
    val Error': Typ.t -> exp
    val Int': Int.t -> exp
    val Plus': exp * exp -> exp
    val LEq': exp * exp -> exp
    val List': Typ.t * exp list -> exp
    val Append': exp * exp -> exp
    val Index': exp * exp -> exp
    val Len': exp -> exp
    val Pair': exp Labeled.t -> exp
    val Proj': exp * Label.t -> exp
    val Inj': Typ.t Labeled.t * Label.t * exp -> exp
    val Case': Typ.t * exp * (expVar * exp) Labeled.t -> exp
    val Lam': (expVar * Typ.t) * exp -> exp
    val Ap': exp * exp -> exp
    val Fold': (Typ.typVar * Typ.t) * exp -> exp
    val Unfold': exp -> exp

    val into: view -> exp
    val out: exp -> view
    val aequiv: exp * exp -> bool
    val toString: exp -> string

    val substTyp: Typ.t -> Typ.Var.t -> exp -> exp
    val subst: exp -> expVar -> exp -> exp
  end
end

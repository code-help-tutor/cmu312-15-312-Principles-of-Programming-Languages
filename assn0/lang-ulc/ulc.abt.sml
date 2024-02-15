structure ULC :> ULC =
struct
  structure Term =
  struct
    type termVar = Variable.t

    datatype term =
      BV of int
    | FV of Variable.t
    | LAM of term
    | AP of term * term
    type t = term

    structure Var = Variable

    datatype view = Var of termVar | Lam of termVar * term | Ap of term * term


    fun into (view: view) : term = raise Fail "Unimplemented"

    fun out (term: term) : view = raise Fail "Unimplemented"

    fun aequiv (term: term, term': term) : bool = raise Fail "Unimplemented"

    fun subst (term1: term) (x: termVar) (term: term) : term =
      raise Fail "Unimplemented"


    val Var' = into o Var
    val Lam' = into o Lam
    val Ap' = into o Ap

    fun toString (term: term) : string =
      case out term of
        Var x => Var.toString x
      | Lam (x, m) =>
          "(Lam " ^ ("(" ^ Var.toString x ^ ", " ^ toString m ^ ")") ^ ")"
      | Ap (m1, m2) =>
          "(Ap " ^ ("(" ^ toString m1 ^ ", " ^ toString m2 ^ ")") ^ ")"
  end
end

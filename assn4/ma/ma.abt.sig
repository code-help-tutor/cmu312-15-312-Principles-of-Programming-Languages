signature MA = sig
  structure Ref : TEMP

  structure String : sig
    type t
  end

  structure Oper : sig
    type t
  end

  structure Int : sig
    type t
  end

  structure Bool : sig
    type t
  end

  structure Typ : sig
    type typVar
    type typ
    type t = typ

    structure Var : TEMP where type t = typVar

    datatype view
      = Var of typVar
      | Nat
      | Bool
      | Unit
      | Void
      | String
      | Arr of typ * typ
      | Star of typ * typ
      | Sum of typ * typ
      | Rec of typVar * typ
      | Cmd of typ

    val Var' : typVar -> typ
    val Nat' : typ
    val Bool' : typ
    val Unit' : typ
    val Void' : typ
    val String' : typ
    val Arr' : typ * typ -> typ
    val Star' : typ * typ -> typ
    val Sum' : typ * typ -> typ
    val Rec' : typVar * typ -> typ
    val Cmd' : typ -> typ

    val into : view -> typ
    val out : typ -> view
    val aequiv : typ * typ -> bool
    val toString : typ -> string

    val subst : typ -> typVar -> typ -> typ
  end

  structure Exp : sig
    type cmd
    type expVar
    type exp
    type t = exp

    structure Var : TEMP where type t = expVar

    datatype view
      = Var of expVar
      | String of String.t
      | Num of Int.t
      | Succ of exp
      | Ifz of exp * exp * (expVar * exp)
      | Bool of Bool.t
      | Negate of exp
      | If of exp * (exp * exp)
      | Binop of Oper.t * exp * exp
      | Fun of (Typ.t * Typ.t) * ((expVar * expVar) * exp)
      | Lam of Typ.t * (expVar * exp)
      | App of exp * exp
      | Let of exp * (expVar * exp)
      | Triv
      | Pair of exp * exp
      | Split of exp * ((expVar * expVar) * exp)
      | Inl of Typ.t * exp
      | Inr of Typ.t * exp
      | Case of exp * (expVar * exp) * (expVar * exp)
      | Abort of Typ.t * exp
      | Fold of Typ.t * exp
      | Unfold of exp
      | Cmd of cmd

    val Var' : expVar -> exp
    val String' : String.t -> exp
    val Num' : Int.t -> exp
    val Succ' : exp -> exp
    val Ifz' : exp * exp * (expVar * exp) -> exp
    val Bool' : Bool.t -> exp
    val Negate' : exp -> exp
    val If' : exp * (exp * exp) -> exp
    val Binop' : Oper.t * exp * exp -> exp
    val Fun' : (Typ.t * Typ.t) * ((expVar * expVar) * exp) -> exp
    val Lam' : Typ.t * (expVar * exp) -> exp
    val App' : exp * exp -> exp
    val Let' : exp * (expVar * exp) -> exp
    val Triv' : exp
    val Pair' : exp * exp -> exp
    val Split' : exp * ((expVar * expVar) * exp) -> exp
    val Inl' : Typ.t * exp -> exp
    val Inr' : Typ.t * exp -> exp
    val Case' : exp * (expVar * exp) * (expVar * exp) -> exp
    val Abort' : Typ.t * exp -> exp
    val Fold' : Typ.t * exp -> exp
    val Unfold' : exp -> exp
    val Cmd' : cmd -> exp

    val into : view -> exp
    val out : exp -> view
    val aequiv : exp * exp -> bool
    val toString : exp -> string

    val substTyp : Typ.t -> Typ.Var.t -> exp -> exp
    val subst : exp -> expVar -> exp -> exp
  end

  structure Cmd : sig
    type exp
    type expVar
    type cmd
    type t = cmd

    datatype view
      = Ret of exp
      | Bind of exp * (expVar * cmd)
      | Decl of exp * (Ref.t * cmd)
      | Get of Ref.t
      | Set of Ref.t * exp
      | Print of exp

    val Ret' : exp -> cmd
    val Bind' : exp * (expVar * cmd) -> cmd
    val Decl' : exp * (Ref.t * cmd) -> cmd
    val Get' : Ref.t -> cmd
    val Set' : Ref.t * exp -> cmd
    val Print' : exp -> cmd

    val into : view -> cmd
    val out : cmd -> view
    val aequiv : cmd * cmd -> bool
    val toString : cmd -> string

    val substTyp : Typ.t -> Typ.Var.t -> cmd -> cmd
    val substExp : exp -> expVar -> cmd -> cmd
  end

  sharing type Exp.exp = Cmd.exp

  sharing type Exp.cmd = Cmd.cmd

  sharing type Exp.expVar = Cmd.expVar
end

signature KPCFV =
sig

  structure Typ:
  sig
    type typ
    type t = typ

    datatype view =
      Comp of typ
    | Cont of typ
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

    val Comp': typ -> typ
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

  structure Value:
  sig
    type stack
    type exp
    type valueVar = Variable.t
    type value
    type t = value

    structure Var: TEMP where type t = valueVar

    datatype view =
      Var of valueVar
    | Comp of exp
    | Cont of Typ.t * stack
    | Unit
    | Tuple of value * value
    | InjL of (Typ.t * Typ.t) * value
    | InjR of (Typ.t * Typ.t) * value
    | Fun of (Typ.t * Typ.t) * ((valueVar * valueVar) * exp)
    | Lam of (valueVar * Typ.t) * exp
    | Zero
    | Succ of value

    val Var': valueVar -> value
    val Comp': exp -> value
    val Cont': Typ.t * stack -> value
    val Unit': value
    val Tuple': value * value -> value
    val InjL': (Typ.t * Typ.t) * value -> value
    val InjR': (Typ.t * Typ.t) * value -> value
    val Fun': (Typ.t * Typ.t) * ((valueVar * valueVar) * exp) -> value
    val Lam': (valueVar * Typ.t) * exp -> value
    val Zero': value
    val Succ': value -> value

    val into: view -> value
    val out: value -> view
    val aequiv: value * value -> bool
    val toString: value -> string

    val subst: value -> valueVar -> value -> value
  end

  structure Stack:
  sig
    type value
    type exp
    type valueVar = Variable.t
    type stack
    type t = stack

    datatype view = Epsilon | Frame of stack * (valueVar * exp)

    val Epsilon': stack
    val Frame': stack * (valueVar * exp) -> stack

    val into: view -> stack
    val out: stack -> view
    val aequiv: stack * stack -> bool
    val toString: stack -> string

    val substValue: value -> valueVar -> stack -> stack
  end

  structure Exp:
  sig
    type value
    type stack
    type valueVar = Variable.t
    type exp
    type t = exp

    datatype view =
      Ret of value
    | Bind of value * (valueVar * exp)
    | Letcc of Typ.t * (valueVar * exp)
    | Throw of Typ.t * value * value
    | Split of value * ((valueVar * valueVar) * exp)
    | Abort of Typ.t * value
    | Case of value * (valueVar * exp) * (valueVar * exp)
    | Ap of value * value
    | Ifz of value * exp * (valueVar * exp)

    val Ret': value -> exp
    val Bind': value * (valueVar * exp) -> exp
    val Letcc': Typ.t * (valueVar * exp) -> exp
    val Throw': Typ.t * value * value -> exp
    val Split': value * ((valueVar * valueVar) * exp) -> exp
    val Abort': Typ.t * value -> exp
    val Case': value * (valueVar * exp) * (valueVar * exp) -> exp
    val Ap': value * value -> exp
    val Ifz': value * exp * (valueVar * exp) -> exp

    val into: view -> exp
    val out: exp -> view
    val aequiv: exp * exp -> bool
    val toString: exp -> string

    val substValue: value -> valueVar -> exp -> exp
  end

  sharing type Value.value = Stack.value

  sharing type Stack.value = Exp.value

  sharing type Value.stack = Stack.stack

  sharing type Stack.stack = Exp.stack

  sharing type Value.exp = Stack.exp

  sharing type Stack.exp = Exp.exp

  structure State:
  sig
    datatype state =
      Eval of Stack.t * Exp.t (* k ▷ e *)
    | Ret of Stack.t * Value.t (* k ◁ v *)

    type t = state

    val aequiv: t * t -> bool

    val toString: t -> string
  end
end

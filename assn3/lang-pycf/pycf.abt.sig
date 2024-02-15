signature PYCF =
sig

  structure Int:
  sig
    type t = int
  end

  structure Bool:
  sig
    type t = bool
  end

  structure Class:
  sig
    datatype class = Bool | Int | List | Fun

    type t = class

    val aequiv: t * t -> bool

    val toString: t -> string
  end

  structure Object:
  sig
    type objectVar = Variable.t
    type object
    type t = object

    structure Var: TEMP where type t = objectVar

    datatype view =
      Var of objectVar
    | Bool of Bool.t
    (* (cond   , then   , else  ) *)
    | If of object * object * object
    | Int of Int.t
    | Plus of object * object
    | LEq of object * object
    | List of object list
    (* (list   , index ) *)
    | Index of object * object
    | Len of object
    | Fun of objectVar * (objectVar * object)
    | Ap of object * object
    | Let of object * (objectVar * object)
    | IsInstance of object * Class.t

    val Var': objectVar -> object
    val Bool': Bool.t -> object
    val If': object * object * object -> object
    val Int': Int.t -> object
    val Plus': object * object -> object
    val LEq': object * object -> object
    val List': object list -> object
    val Index': object * object -> object
    val Len': object -> object
    val Fun': objectVar * (objectVar * object) -> object
    val Ap': object * object -> object
    val Let': object * (objectVar * object) -> object
    val IsInstance': object * Class.t -> object

    val into: view -> object
    val out: object -> view
    val aequiv: object * object -> bool
    val toString: object -> string

    val subst: object -> objectVar -> object -> object
  end
end

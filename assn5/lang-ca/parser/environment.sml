signature ENVIRONMENT =
sig
  type env

  datatype ident =
    BoundTermVariable of CA.Exp.Var.t
  | FreeTermVariable of CA.Exp.Var.t
  | BoundTypeVariable of CA.Typ.Var.t
  | FreeTypeVariable of CA.Typ.Var.t
  | BoundAssignable of CA.Chan.t
  | FreeAssignable of CA.Chan.t

  val toString: ident -> string

  (* Those contexts are for parsing *)
  val identTable: env -> ident Symbols.dict
  val typeContext: env -> CA.Typ.t TypVariableContext.dict
  val termContext: env -> (CA.Exp.t * CA.Typ.t) VariableContext.dict
  val assignableContext: env -> CA.Typ.t SymbolContext.dict

  val addTerm: CA.Exp.Var.t -> (CA.Exp.t * CA.Typ.t) -> env -> env
  val addType: CA.Typ.Var.t -> CA.Typ.t -> env -> env
  val addAssignable: CA.Chan.t -> CA.Typ.t -> env -> env

  val newenv: env
end

structure Environment: ENVIRONMENT =
struct
  datatype ident =
    BoundTermVariable of CA.Exp.Var.t
  | FreeTermVariable of CA.Exp.Var.t
  | BoundTypeVariable of CA.Typ.Var.t
  | FreeTypeVariable of CA.Typ.Var.t
  | BoundAssignable of CA.Chan.t
  | FreeAssignable of CA.Chan.t

  fun toString (BoundTermVariable v) = CA.Exp.Var.toUserString v
    | toString (FreeTermVariable v) = CA.Exp.Var.toUserString v
    | toString (BoundTypeVariable v) = CA.Typ.Var.toUserString v
    | toString (FreeTypeVariable v) = CA.Typ.Var.toUserString v
    | toString (BoundAssignable s) = CA.Chan.toUserString s
    | toString (FreeAssignable s) = CA.Chan.toUserString s

  datatype env =
    Env of
      ident Symbols.dict
      * (CA.Exp.t * CA.Typ.t) VariableContext.dict
      * CA.Typ.t TypVariableContext.dict
      * CA.Typ.t SymbolContext.dict

  fun identTable (Env (sym, _, _, _)) = sym
  fun termContext (Env (_, term, _, _)) = term
  fun typeContext (Env (_, _, typ, _)) = typ
  fun assignableContext (Env (_, _, _, a)) = a

  fun insertVar sym var symtype =
    Symbols.insert sym (CA.Exp.Var.toUserString var) (symtype var)
  fun insertTypVar sym var symtype =
    Symbols.insert sym (CA.Typ.Var.toUserString var) (symtype var)
  fun insertSym sym var symtype =
    Symbols.insert sym (CA.Chan.toUserString var) (symtype var)

  fun addTerm var e (Env (sym, term, typ, a)) =
    Env
      ( insertVar sym var FreeTermVariable
      , VariableContext.insert term var e
      , typ
      , a
      )
  fun addType var e (Env (sym, term, typ, a)) =
    Env
      ( insertTypVar sym var FreeTypeVariable
      , term
      , TypVariableContext.insert typ var e
      , a
      )
  fun addAssignable var e (Env (sym, term, typ, a)) =
    Env
      ( insertSym sym var FreeAssignable
      , term
      , typ
      , SymbolContext.insert a var e
      )
  val newenv =
    Env
      ( Symbols.empty
      , VariableContext.empty
      , TypVariableContext.empty
      , SymbolContext.empty
      )
end

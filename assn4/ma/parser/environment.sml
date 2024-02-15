signature ENVIRONMENT =
sig
  type env

  datatype ident =
    BoundTermVariable of MA.Exp.Var.t
  | FreeTermVariable of MA.Exp.Var.t
  | BoundTypeVariable of MA.Typ.Var.t
  | FreeTypeVariable of MA.Typ.Var.t
  | BoundAssignable of MA.Ref.t
  | FreeAssignable of MA.Ref.t

  val toString : ident -> string

  (* Those contexts are for parsing *)
  val identTable : env -> ident Symbols.dict
  val typeContext : env -> MA.Typ.t TypVariableContext.dict
  val termContext : env -> (MA.Exp.t * MA.Typ.t) VariableContext.dict
  val assignableContext : env -> MA.Typ.t SymbolContext.dict

  val addTerm : MA.Exp.Var.t -> (MA.Exp.t * MA.Typ.t) -> env -> env
  val addType : MA.Typ.Var.t -> MA.Typ.t -> env -> env
  val addAssignable : MA.Ref.t -> MA.Typ.t -> env -> env

  val newenv : env
end

structure Environment : ENVIRONMENT =
struct
  datatype ident =
    BoundTermVariable of MA.Exp.Var.t
  | FreeTermVariable of MA.Exp.Var.t
  | BoundTypeVariable of MA.Typ.Var.t
  | FreeTypeVariable of MA.Typ.Var.t
  | BoundAssignable of MA.Ref.t
  | FreeAssignable of MA.Ref.t

  fun toString (BoundTermVariable v) = MA.Exp.Var.toUserString v
    | toString (FreeTermVariable v) = MA.Exp.Var.toUserString v
    | toString (BoundTypeVariable v) = MA.Typ.Var.toUserString v
    | toString (FreeTypeVariable v) = MA.Typ.Var.toUserString v
    | toString (BoundAssignable s) = MA.Ref.toUserString s
    | toString (FreeAssignable s) = MA.Ref.toUserString s

  datatype env = Env of
    ident Symbols.dict *
    (MA.Exp.t * MA.Typ.t) VariableContext.dict *
    MA.Typ.t TypVariableContext.dict *
    MA.Typ.t SymbolContext.dict

  fun identTable (Env (sym, _, _, _)) = sym
  fun termContext (Env (_, term, _, _)) = term
  fun typeContext (Env (_, _, typ, _)) = typ
  fun assignableContext (Env (_, _, _, a)) = a

  fun insertVar sym var symtype = Symbols.insert sym (MA.Exp.Var.toUserString var) (symtype var)
  fun insertTypVar sym var symtype = Symbols.insert sym (MA.Typ.Var.toUserString var) (symtype var)
  fun insertSym sym var symtype = Symbols.insert sym (MA.Ref.toUserString var) (symtype var)

  fun addTerm var e (Env (sym, term, typ, a)) =
    Env (
      insertVar sym var FreeTermVariable,
      VariableContext.insert term var e,
      typ,
      a
    )
  fun addType var e (Env (sym, term, typ, a)) =
    Env (
      insertTypVar sym var FreeTypeVariable,
      term,
      TypVariableContext.insert typ var e,
      a
    )
  fun addAssignable var e (Env (sym, term, typ, a)) =
    Env (
      insertSym sym var FreeAssignable,
      term,
      typ,
      SymbolContext.insert a var e
    )
  val newenv =
      Env (Symbols.empty, VariableContext.empty, TypVariableContext.empty, SymbolContext.empty)
end

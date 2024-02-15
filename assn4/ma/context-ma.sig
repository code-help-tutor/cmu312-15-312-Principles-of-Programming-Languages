signature CONTEXT_MA =
  sig
    include CONTEXT

    type typVar = MA.Typ.Var.t

    val singletonTyp : typVar -> context
    val insertTyp : context -> typVar -> context
    val findTyp : context -> typVar -> bool

    type expVar = MA.Exp.Var.t
    type typ = MA.Typ.t

    val singletonExp : expVar -> typ -> context
    val insertExp : context -> expVar -> typ -> context
    val findExp : context -> expVar -> typ option

    type ref = MA.Ref.t

    val singletonRef : ref -> typ -> context
    val insertRef : context -> ref -> typ -> context
    val findRef : context -> ref -> typ option

  end

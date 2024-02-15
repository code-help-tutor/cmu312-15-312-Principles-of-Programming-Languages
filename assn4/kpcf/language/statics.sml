structure Statics :> STATICS =
struct
  open KPCFv

  exception TypeError of string

  type context = Typ.t Context.dict

  fun checkStack (k: Stack.t) (typ: Typ.t) : unit = raise Fail "Unimplemented"

  and inferTypeValue (ctx: context) (value: Value.t) : Typ.t =
    raise Fail "Unimplemented"

  and checkTypeValue (ctx: context) (value: Value.t) (typ: Typ.t) : unit =
    raise Fail "Unimplemented"

  and inferTypeExp (ctx: context) (exp: Exp.t) : Typ.t =
    raise Fail "Unimplemented"

  and checkTypeExp (ctx: context) (exp: Exp.t) (typ: Typ.t) : unit =
    raise Fail "Unimplemented"

  fun checkState (state: State.t) : unit = raise Fail "Unimplemented"
end

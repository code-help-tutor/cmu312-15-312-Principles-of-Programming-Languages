structure Dynamics :> DYNAMICS =
struct
  open KPCFv

  exception Malformed

  datatype transition = Step of State.t | Final of Value.t

  fun progress (state: State.t) : transition = raise Fail "Unimplemented"
end

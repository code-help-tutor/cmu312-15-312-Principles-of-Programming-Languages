structure SynExt :> SYN_EXT =
struct
  open SynExtHelpers

  (* Recommendation: use `classToSummand Dyn class` to get the
     label and type of class `class`. *)

  fun new (class: class) (e: exp) : exp = raise Fail "Unimplemented"

  fun cast (e: exp) (class: class) : exp = raise Fail "Unimplemented"

  fun Fun (tau1: typ, tau2: typ) (f: expVar, x: expVar, e: exp) : exp =
    raise Fail "Unimplemented"
end

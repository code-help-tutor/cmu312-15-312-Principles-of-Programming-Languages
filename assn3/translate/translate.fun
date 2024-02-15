functor Translate(SynExt: SYN_EXT) :> TRANSLATE =
struct
  open PyCF FPC
  open SynExt

  fun translate (object: Object.t) : Exp.t = raise Fail "Unimplemented"
end

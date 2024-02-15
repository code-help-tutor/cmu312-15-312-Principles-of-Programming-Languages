functor NormalizeULC(ULC: ULC) :> NORMALIZE_ULC where type term = ULC.Term.t =
struct
  structure Term = ULC.Term
  type term = Term.t

  (* weak head normal form *)
  fun whnf (term: Term.t) : Term.t = raise Fail "Unimplemented"

  fun norm (term: Term.t) : Term.t = raise Fail "Unimplemented"
end

functor ContEvaluator(Dynamics: DYNAMICS) :>
  EVALUATOR
  where type term = Dynamics.term Dynamics.State.t
  and type final = Void.void Dynamics.State.t =
struct
  type term = Dynamics.term Dynamics.State.t
  type final = Void.void Dynamics.State.t

  fun evaluate (term: Dynamics.term Dynamics.State.t) : final =
    let
      val opt: Void.void Dynamics.State.t option = Cont.callcc (fn k =>
        SOME (Dynamics.State.map (fn _ => Cont.throw k NONE) term))
    in
      case opt of
        NONE => evaluate (Dynamics.State.bind (term, Dynamics.progress))
      | SOME final => final
    end
end

signature DYNAMICS =
sig
  structure State: STATE (* parameter *)

  type term (* parameter *)

  structure Error: SHOW (* abstract *)
  exception Malformed of Error.t

  (** `progress term`
   *
   * Step `term` to a machine state.
   * If `term` is malformed, raise `Malformed error`, for some `error : Error.t`.
   *)
  val progress: term -> term State.t
end

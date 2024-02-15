signature STATICS =
sig
  structure Typ: SHOW (* parameter *)
  structure Term: SHOW (* parameter *)

  structure Context: CONTEXT (* parameter *)

  structure Error: SHOW (* abstract *)
  exception TypeError of Error.t

  (** `inferType context term`
   *
   * Infer the type of `term` in `context`.
   * Otherwise, raise `TypeError error`, for some `error : Error.t`.
   *)
  val inferType: Context.t -> Term.t -> Typ.t

  (** `checkType context term typ`
   *
   * Check that the type of `term` in `context` is `typ`, returning `()`.
   * Otherwise, raise `TypeError error`, for some `error : Error.t`.
   *)
  and checkType: Context.t -> Term.t -> Typ.t -> unit
end

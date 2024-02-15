signature PARSE_TERM =
sig
  structure ULC: ULC

  datatype t =
    Var of string
  | App of t * t
  | Lam of string * t
  | Norm of t
  | Normed of ULC.Term.t

  (* add definitions from a symbol table *)
  val add_defs: t -> t Symbols.dict -> t

  (* convert to a term using a normalization function *)
  val to_term: t -> (ULC.Term.t -> ULC.Term.t) -> ULC.Term.t

  (* normalize fully, but keep as a parse term for later manipulation *)
  val norm: t -> (ULC.Term.t -> ULC.Term.t) -> t
end

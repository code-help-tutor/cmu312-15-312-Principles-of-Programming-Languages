signature PARSER =
sig
  structure ULC: ULC
  structure ParseTerm: PARSE_TERM
  sharing ULC = ParseTerm.ULC
  structure Stmt: STMT where type term = ParseTerm.t

  datatype position = EOF | Pos of int

  exception ParseError of position

  val parse: (ULC.Term.t -> ULC.Term.t)
             -> char Stream.stream
             -> ParseTerm.t Symbols.dict
             -> (Stmt.t list * ParseTerm.t Symbols.dict)
end

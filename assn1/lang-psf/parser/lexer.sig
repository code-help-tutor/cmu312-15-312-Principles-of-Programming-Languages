signature LEXER =
sig
  exception Error
  type token

  type pos = int * int
  val posToString: pos -> string

  val lex: char Stream.stream -> (token * pos) Stream.stream
end

signature PARSER =
sig
  datatype position = EOF | Pos of int * int

  exception ParseError of position * string option * string
  exception IncorrectSortError

  val parse: char Stream.stream -> KPCF.Exp.t
  val parseFile: string -> KPCF.Exp.t
end

signature PARSER_CA =
sig
  type cmd

  datatype position = EOF | Pos of int

  exception ParseError of position

  val parse: Environment.env -> char Stream.stream -> cmd
end

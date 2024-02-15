signature PARSER =
sig
  type directive

  datatype result = Ok of directive | Err of string

  val parse: char Stream.stream -> result Stream.stream
end

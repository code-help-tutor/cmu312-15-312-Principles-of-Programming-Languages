signature INTERPRETER_IMPL =
sig
  structure Parser: PARSER

  type state
  val init: state

  val runDirective: Parser.directive -> state -> state
end

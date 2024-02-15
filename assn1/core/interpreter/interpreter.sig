signature INTERPRETER =
sig
  val repl: unit -> unit
  val evalFile: string -> unit
end

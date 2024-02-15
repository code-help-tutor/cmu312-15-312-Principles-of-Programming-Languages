signature TOPLEVEL =
sig
  (* launch the repl *)
  val repl: unit -> unit

  (* evaluate a file *)
  val evalFile: string -> unit
end

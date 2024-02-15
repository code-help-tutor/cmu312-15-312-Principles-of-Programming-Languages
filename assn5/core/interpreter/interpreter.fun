functor Interpreter(Impl: INTERPRETER_IMPL) :> INTERPRETER =
struct
  structure Parser = Impl.Parser

  local
    fun foldl (f: 'a * 'b -> 'b) (e: 'b) (s: 'a Stream.stream) : 'b =
      case Stream.front s of
        Stream.Nil => e
      | Stream.Cons (x, s') => foldl f (f (x, e)) s'
  in
    val session =
      ignore
      o
      foldl
        (fn (directive, state) =>
           case directive of
             Parser.Ok d => Impl.runDirective d state
           | Parser.Err s => (TextIO.print (s ^ "\n"); state)) Impl.init
  end

  fun repl () =
    session (Input.promptKeybd "-> " "=> " (Parser.parse o Stream.map #1))

  local
    fun parseFile f fname =
      let
        val ins = TextIO.openIn fname
        val e = f (Stream.fromTextInstream ins)
                handle exn => (TextIO.closeIn ins; raise exn)
        val () = TextIO.closeIn ins
      in
        e
      end
  in val evalFile = parseFile (session o Parser.parse)
  end
end

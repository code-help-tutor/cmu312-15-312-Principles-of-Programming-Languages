structure InterpreterKPCF :> INTERPRETER_KPCF =
struct
  structure Var = Variable

  (***** Convert a charstream into a string stream, breaking at ; boundaries ****)
  local
    fun revtostring L =
      String.implode (foldl (op::) [] L)
  in
    fun stringStream L s =
      case Stream.front s of
        Stream.Cons ((#";", _), s) =>
          Stream.lazy (fn () =>
            Stream.Cons (revtostring L, Stream.lazy (fn () =>
              Stream.front (stringStream [] s))))
      | Stream.Cons ((c, _), s) => stringStream (c :: L) s
      | Stream.Nil =>
          if Input.isBlank (String.implode L) then (* avoid parser errors at EOF *)
            Stream.lazy (fn () => Stream.Nil)
          else
            Stream.lazy (fn () =>
              Stream.Cons (revtostring L, Stream.lazy (fn () => Stream.Nil)))
  end

  fun trim s =
    if String.size s < 79 then (s ^ "\n")
    else (String.substring (s, 0, 76) ^ "...\n")

  fun foldl f x s =
    case Stream.front s of
      Stream.Nil => x
    | Stream.Cons (h, t) => foldl f (f (h, x)) t

  fun parse text =
    Parser.parse (Stream.fromString text)

  exception Impossible


  fun check e =
    let
      val typ = Statics.inferTypeExp Context.empty e
    in
      typ
      before
      TextIO.print ("Statics: term has type " ^ KPCFv.Typ.toString typ ^ "\n")
    end

  fun checkFile filename =
    foldl (fn (text, _) => SOME (check (Elaborator.elaborateExp (parse text))))
      NONE (stringStream [] (Input.readFile filename))


  fun evalv e =
    let
      val typ = Statics.inferTypeExp Context.empty e
      val _ = TextIO.print (trim
        ("Statics: term has type " ^ KPCFv.Typ.toString typ))
      val initState = KPCFv.State.Eval (KPCFv.Stack.Epsilon', e)

      fun eval (state: KPCFv.State.t) : KPCFv.Value.t =
        case Dynamics.progress state of
          Dynamics.Step state' => eval state'
        | Dynamics.Final value => value
    in
      eval initState
    end

  val eval = evalv o Elaborator.elaborateExp

  fun evalFile filename =
    foldl (fn (text, _) => SOME (eval (parse text))) NONE
      (stringStream [] (Input.readFile filename))

  fun hdl f x =
    (f x)
    handle
      Parser.ParseError (Parser.EOF, _, e) =>
        TextIO.print ("Syntax error at end of file. " ^ e ^ "\n")
    | Parser.ParseError (Parser.Pos (l, c), SOME token, e) =>
        TextIO.print
          ("Unexpected token " ^ token ^ " at line " ^ Int.toString l
           ^ ", column " ^ Int.toString c ^ ". " ^ e ^ "\n")
    | Parser.ParseError (Parser.Pos (l, c), NONE, _) =>
        TextIO.print
          ("Syntax error at line " ^ Int.toString l ^ ", column "
           ^ Int.toString c ^ ".\n")
    | Parser.IncorrectSortError =>
        TextIO.print
          "Error: expressions are expected at top level, not values.\n"
    | ParserState.UnknownIdError s => TextIO.print (s ^ "\n")
    | Statics.TypeError s => TextIO.print ("Type error: " ^ s ^ "\n")
    | Dynamics.Malformed => TextIO.print "Malformed error"
    | e => TextIO.print ("Exception: " ^ General.exnMessage e ^ "\n")

  fun process text =
    let val s = eval (parse text)
    in TextIO.print (KPCFv.Value.toString s ^ "\n")
    end

  fun run (text, ()) = hdl process text

  fun repl () =
    (foldl run () (Input.promptKeybd "->" "=>" (stringStream [])); ())
end

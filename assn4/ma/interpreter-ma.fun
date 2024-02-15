functor InterpreterMA (structure Dynamics : DYNAMICS where State = StateMA and type term = MA.Cmd.t
                       structure Parser : PARSER_MA where type cmd = TopLevelCommands.cmd)
        :> INTERPRETER =
struct
  open TopLevelCommands
  structure E = Environment
  structure T = MA.Typ
  structure TE = MA.Exp

  exception InternalError of string
  exception PreservationError of string
  exception InvalidTopLevelCommand
  exception ParseError of string * Parser.position

  (***** Convert a charstream into a string stream, breaking at ; boundaries ****)
  local
    fun revtostring L = String.implode (foldl (op ::) [] L)
  in
    fun stringStream L s = case Stream.front s of
      Stream.Cons ((#";", _), s) =>
        Stream.lazy (fn () =>
          Stream.Cons (revtostring L, Stream.lazy (fn () => Stream.front (stringStream [] s)))
        )
    | Stream.Cons ((c, _), s) => stringStream (c::L) s
    | Stream.Nil =>
        if Input.isBlank (String.implode L) then (* avoid parser errors at EOF *)
          Stream.lazy (fn () => Stream.Nil)
        else
          Stream.lazy (fn () => Stream.Cons (revtostring L, Stream.lazy (fn () => Stream.Nil)))
  end

  fun resToString (TypeDefRes (var, res)) =
        "type " ^ T.Var.toUserString var ^ " = " ^ T.toString res ^ "\n"
    | resToString (TermDefRes (var, res, typ)) =
        "val " ^ TE.Var.toUserString var ^ " = " ^ TE.toString res ^ " : " ^ T.toString typ ^ "\n"
    | resToString LoadRes = "OK\n"
    | resToString (CommandRes e) = MA.Exp.toString e ^ "\n"

  structure Evaluator = ContEvaluator (DynamicsMA)

  fun evalCmd cmd =
    let
        val _ = StaticsMA.Cmd.inferType ContextMA.empty cmd
        fun eval cmd mem =
            case DynamicsMA.progress cmd mem of
                StateMA.Step (cmd', mem') => eval cmd' mem'
              | StateMA.Final e => e
    in
        (* case Evaluator.evaluate (StateMA.initial cmd) StateMA.MemDict.empty of *)
        (*     StateMA.Step (void, _) => Void.absurd void *)
        (*   | StateMA.Final e => e *)

        eval cmd StateMA.MemDict.empty
    end

  fun evalTermCore term =
    case DynamicsMAExp.progress term of
        StateMAExp.Step term' => evalTermCore term'
      | StateMAExp.Val _ => term

  fun evalTerm term =
    let
      val typ = StaticsMA.Exp.inferType ContextMA.empty term
      val term' = evalTermCore term
    in
      (term', typ)
    end

  fun foldl f x s =
    case Stream.front s of
      Stream.Nil => x
    | Stream.Cons (h, t) => foldl f (f (h, x)) t

  fun replacePos p y = String.substring (y, 0, p) ^ "$\n"

  val handler =
   fn ParseError (_, Parser.EOF) =>
        (TextIO.print "Syntax error at end of file"; ())
    | ParseError (text, Parser.Pos pos) =>
        (TextIO.print ("Syntax error at " ^ Int.toString pos ^ "\n");
         TextIO.print (replacePos pos text); ())
    | ParserState.UnknownIdError s =>
       (TextIO.print ("Parse error:" ^ s ^ "\n"); ())
    | StaticsMA.Exp.TypeError e =>
       (TextIO.print ("Statics type error:" ^ StaticsMA.Exp.Error.toString e ^ "\n"); ())
    | StaticsMA.Cmd.TypeError e =>
       (TextIO.print ("Statics type error:" ^ StaticsMA.Cmd.Error.toString e ^ "\n"); ())
    | InternalError e =>
       (TextIO.print ("Internal error:" ^ e ^ "\n"); ())
    | e => ((TextIO.print ("Exception: " ^ General.exnMessage e ^ "\n")); ())


  fun evalCore repl env parseRes =
    case parseRes of
      TypeDef (var, typ) => (TypeDefRes (var, typ), [], E.addType var typ env)
    | TermDef (var, exp) =>
        let
            val (exp', typ) = evalTerm exp
            val env = E.addTerm var (exp, typ) env
        in
            (TermDefRes (var, exp', typ), [], env)
        end
    | Command cmd => (CommandRes (evalCmd cmd), [], env)
    | Load filename =>
      let
        val (a, env') = evalFile' repl filename env
      in
          (LoadRes, a, env')
      end

  and processDef repl text (res, a, env) =
     let
        val parseRes =
          Parser.parse env (Stream.fromString text)
            handle Parser.ParseError pos => raise ParseError (text, pos)
        val (res', a', env') = evalCore repl env parseRes
     in
        (SOME res', a' @ a, env')
     end

  and processDefRepl text a =
     let
         val (res, ass, env') = processDef true text a
     in
       case res of
            SOME r => (TextIO.print (resToString r) ; (res, ass, env'))
         |  NONE => raise InternalError "No result in processDefRepl"
     end

  and evalFile' repl filename env =
    let
      fun foo (text, s) =
        if repl then
          (print ("-> " ^ text ^ "\n"); processDefRepl text s)
        else
          processDef false text s
      val (r, a, env) =
        foldl foo (NONE, [], env) (stringStream [] (Input.readFile (OS.Path.concat ("tests", filename))))
    in
      case r of
        NONE => (a, env)
      | SOME r => (TextIO.print (resToString r); (a, env))
  end

  val newenv =
    let
      val env = E.newenv
      val env = E.addType (T.Var.new "bool") T.Bool' env
      val env = E.addType (T.Var.new "nat") T.Nat' env
      val env = E.addType (T.Var.new "str") T.String' env
    in
      env
    end

  fun hdl f x y = (f x y)
    handle e => (handler e; y)

  fun evalFile filename = ignore (evalFile' false filename newenv
      handle e => (handler e; raise e))

  (* The REPL does not collect failed assertions *)
  fun dropAssertions (text, (r, env)) =
    let
      val (r', a, env') = (hdl processDefRepl text (r, [], env))
    in
      (r', env')
    end

  fun repl () =
     (foldl dropAssertions (NONE, newenv)
        (Input.promptKeybd "->" "=>" (stringStream [])); ())

end

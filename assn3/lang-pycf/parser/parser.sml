structure ParserPyCF :> PARSER where type directive = PyCF.Object.t =
struct
  type directive = PyCF.Object.t

  datatype result = Ok of directive | Err of string

  datatype error = EOF | Pos of Token.token * Lexer.pos

  exception ParseError of error

  structure ParserState = ParserState(PyCF.Object.Var)

  structure Arg =
  let
    fun lift x () = x

    open ParserState
    structure MU = MonadUtil(ParserState)
    open MU
    infix 4 >>=
    infix 4 >>

    open PyCF
  in
    struct
      type string = string
      type int = int
      type bool = bool


      type class = Class.t

      val class_bool = lift Class.Bool
      val class_int = lift Class.Int
      val class_list = lift Class.List
      val class_fun = lift Class.Fun


      type objectseq = Object.t list monad

      val objectseq_nil = liftM0 (nil : Object.t list)
      val objectseq_cons =
        liftM2 (op:: : Object.t * Object.t list -> Object.t list)


      type object = Object.t monad

      val object_id = Fn.id
      fun object_var id =
        lookup id >>= (fn v => return (Object.Var' v))
      val object_bool = return o Object.Bool'
      val object_if = liftM3 Object.If'
      val object_int = return o Object.Int'
      val object_plus = liftM2 Object.Plus'
      val object_leq = liftM2 Object.LEq'
      val object_nil = lift (return (Object.List' nil))
      val object_cons = liftM2 (Object.List' o op::)
      val object_index = liftM2 Object.Index'
      val object_len = liftM1 Object.Len'
      fun object_lam (x, d) =
        withNewVar x (fn x =>
          d >>= (fn d => return (Object.Fun' (Object.Var.new "lambda", (x, d)))))
      val object_ap = liftM2 Object.Ap'
      fun object_isinstance (d, c) =
        d >>= (fn d => return (Object.IsInstance' (d, c)))


      type decl = object -> object
      type declseq = decl

      fun declseq_nil () = Fn.id
      val declseq_cons = op o
      fun decl_def (f, arg, construct_decls, ret_object) acc =
        withNewVar f (fn f =>
          withNewVar arg (fn arg =>
            construct_decls ret_object
            >>= (fn d => return (Object.Fun' (f, (arg, d))))))
        >>=
        (fn def =>
           withNewVar f (fn f =>
             acc >>= (fn acc => return (Object.Let' (def, (f, acc))))))
      fun decl_assign (x, d) acc =
        d
        >>=
        (fn d =>
           withNewVar x (fn x =>
             acc >>= (fn acc => return (Object.Let' (d, (x, acc))))))


      type prog = object

      val prog_id = Fn.id
      fun prog_mk (decls, body) = decls body


      datatype terminal = datatype Token.token


      fun error s =
        case Stream.front s of
          Stream.Nil => ParseError EOF
        | Stream.Cons ((tok, pos), _) => ParseError (Pos (tok, pos))
    end
  end

  structure StreamWithPos =
    CoercedStreamable
      (structure Streamable = StreamStreamable
       type 'a item = 'a * Lexer.pos
       fun coerce (x, _) = x)

  structure ParseMain =
    ParseMain (structure Streamable = StreamWithPos structure Arg = Arg)

  fun splitBySemicolon (stream: Token.token StreamWithPos.t) :
    Token.token StreamWithPos.t Stream.stream =
    Stream.lazy (fn () =>
      let
        fun empty () = Stream.eager Stream.Nil
        fun cons (x, s) =
          Stream.eager (Stream.Cons (x, s))

        fun loop stream =
          case Stream.front stream of
            Stream.Nil => Stream.Nil
          | Stream.Cons ((tok, pos), stream') =>
              (case tok of
                 Token.SEMICOLON =>
                   Stream.Cons (empty (), splitBySemicolon stream')
               | _ =>
                   (case loop stream' of
                      Stream.Nil =>
                        Stream.Cons (cons ((tok, pos), empty ()), empty ())
                    | Stream.Cons (toks, stream'') =>
                        Stream.Cons (cons ((tok, pos), toks), stream'')))
      in
        loop stream
      end)

  fun handler f x =
    Ok (f x)
    handle
      ParserState.UnknownIdError x => Err ("Unbound variable: " ^ x)
    | ParseError error =>
        Err
          (case error of
             EOF => "Parse error: end of file"
           | Pos (_, pos) => "Parse error at " ^ Lexer.posToString pos)

  val parse =
    Stream.map (handler (ParserState.outState o #1 o ParseMain.parse))
    o splitBySemicolon o Lexer.lex
end

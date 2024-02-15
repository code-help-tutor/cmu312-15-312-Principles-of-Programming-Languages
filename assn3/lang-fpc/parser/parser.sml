structure ParserFPC :> PARSER where type directive = FPC.Exp.t =
struct
  type directive = FPC.Exp.t

  datatype result = Ok of directive | Err of string

  datatype error = EOF | Pos of Token.token * Lexer.pos

  exception ParseError of error

  (*
    structure ParserState = ParserState (FPC.Exp.Var)

    structure Arg =
      let
        fun lift x () = x

        open ParserState
        structure MU = MonadUtil (ParserState)
        open MU
        infix 4 >>=
        infix 4 >>

        open FPC
      in
        struct
          type unittp = unit
          val unittm = lift ()


          type string = string
          type int = int


          type typ = Typ.t monad

          val typ_id = Fn.id
          val typ_int = liftM0 Typ.Int'
          (* fun typ_list *)
          fun typ_prod typs = typs >>= (return o Typ.Prod')
          fun typ_sum typs = typs >>= (return o Typ.Sum')
          val typ_arrow = liftM2 Typ.Arrow'
          fun typ_rec (t, tau) =
            withNewVar t (fn t =>
              tau >>= (fn tau =>
                return (Typ.Rec' ((* HACK *) Unsafe.cast t, tau))))

          val

          type tyfield = Label.t * typ
          type tyfieldseq = Typ.t Labeled.t monad

          val tyfield_mk = Fn.id
          fun tyfieldseq_nil () = return Labeled.empty
          fun tyfieldseq_cons ((l, t), ls) =
            ls >>= (fn ls =>
              t >>= (fn t =>
                let
                  val (ls', b) = Labeled.insert' ls l t
                in
                  if b
                    then raise Fail ("duplicate label parsed: " ^ Label.toString l)
                    else return ls'
                end))


          type exp = Exp.t monad

          (*


          type typ = Typ.t

          val typ_id = Fn.id
          val typ_arrow = Typ.Arrow
          val typ_nat = lift Typ.Nat


          type exp = Exp.t monad

          val exp_id = Fn.id
          fun exp_var id =
            lookup id >>= (fn v => return (Exp.Var' v))
          val exp_zero = lift (return Exp.Zero')
          val exp_succ = liftM Exp.Succ'
          fun exp_number n = return (Fn.repeat n Exp.Succ' Exp.Zero')
          fun exp_ifz (e, e0, x, e1) =
            liftM3
              Exp.Ifz'
              ( e
              , e0
              , withNewVar x (fn x =>
                  e1 >>= (fn e1 =>
                    return (x, e1)))
              )
          fun exp_fun (tau1, tau2, f, x, e) =
            withNewVar f (fn f =>
              withNewVar x (fn x =>
                e >>= (fn e =>
                  return (Exp.Fun' ((tau1, tau2), (f, (x, e)))))))
          val exp_ap = liftM2 Exp.Ap' *)


          datatype terminal = datatype Token.token


          fun error s =
            case Stream.front s of
              Stream.Nil => ParseError EOF
            | Stream.Cons ((tok, pos), _) => ParseError (Pos (tok, pos))
        end
      end

    structure StreamWithPos =
      CoercedStreamable (structure Streamable = StreamStreamable
                         type 'a item = 'a * Lexer.pos
                         fun coerce (x, _) = x)

    structure ParseMain =
      ParseMain
        (structure Streamable = StreamWithPos
         structure Arg = Arg)

    fun splitBySemicolon (stream : Token.token StreamWithPos.t) : Token.token StreamWithPos.t Stream.stream =
      Stream.lazy (fn () =>
        let
          fun empty () = Stream.eager Stream.Nil
          fun cons (x, s) = Stream.eager (Stream.Cons (x, s))

          fun loop stream =
            case Stream.front stream of
              Stream.Nil => Stream.Nil
            | Stream.Cons ((tok, pos), stream') => (
                case tok of
                  Token.SEMICOLON => Stream.Cons (empty (), splitBySemicolon stream')
                | _ => (
                    case loop stream' of
                      Stream.Nil => Stream.Cons (cons ((tok, pos), empty ()), empty ())
                    | Stream.Cons (toks, stream'') => Stream.Cons (cons ((tok, pos), toks), stream'')
                  )
              )
        in
          loop stream
        end
      )

    fun handler f x =
      Ok (f x)
        handle ParserState.UnknownIdError x => Err ("Unbound variable: " ^ x)
             | ParseError error =>
                 Err
                  (case error of
                    EOF => "Parse error: end of file"
                  | Pos (_, pos) => "Parse error at " ^ Lexer.posToString pos)

    val parse =
      Stream.map (handler (ParserState.outState o #1 o ParseMain.parse))
      o splitBySemicolon
      o Lexer.lex
  *)

  val parse = fn _ => raise Fail "FPC parser unimplemented"
end

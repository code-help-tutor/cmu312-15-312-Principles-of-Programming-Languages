structure ParserPSF :> PARSER where type directive = PSF.Exp.t =
struct
  type directive = PSF.Exp.t

  datatype result = Ok of directive | Err of string

  datatype error = EOF | Pos of Token.token * Lexer.pos

  exception ParseError of error

  exception UnboundTypeVarError of string

  structure ParserState = ParserState(PSF.Exp.Var)

  structure Arg =
  let
    fun lift x () = x

    open ParserState
    structure MU = MonadUtil(ParserState)
    open MU
    infix 4 >>=
    infix 4 >>

    open PSF
  in
    struct
      type unittp = unit
      val unittm = lift ()

      type string = string
      type int = int

      datatype dir = L | R

      val dir_l = lift L
      val dir_r = lift R


      type typ = Typ.t monad

      val typ_id = Fn.id
      fun typ_var t =
        case t of
          "A" => return Typ.A
        | "B" => return Typ.B
        | "C" => return Typ.C
        | _ => raise UnboundTypeVarError t
      val typ_arrow = liftM2 Typ.Arrow
      val typ_unit = lift (return Typ.Unit)
      val typ_prod = liftM2 Typ.Prod
      val typ_void = lift (return Typ.Void)
      val typ_sum = liftM2 Typ.Sum


      type exp = Exp.t monad

      val exp_id = Fn.id
      fun exp_var id =
        lookup id >>= (fn v => return (Exp.Var' v))
      fun exp_lam (x, t, e) =
        t
        >>=
        (fn t =>
           withNewVar x (fn x => e >>= (fn e => return (Exp.Lam' (t, (x, e))))))
      val exp_ap = liftM2 Exp.Ap'
      val exp_triv = lift (return Exp.Triv')
      val exp_pair = liftM2 Exp.Pair'
      fun exp_pr (e, dir) =
        liftM
          (case dir of
             L => Exp.PrL'
           | R => Exp.PrR') e
      val exp_abort = liftM2 Exp.Abort'
      fun exp_in (dir, tau1, tau2, e) =
        liftM3
          ((case dir of
              L => Exp.InL'
            | R => Exp.InR') o (fn (tau1, tau2, e) => ((tau1, tau2), e)))
          (tau1, tau2, e)
      fun exp_case (tau, e, x1, e1, x2, e2) =
        let
          val branchL = withNewVar x1 (fn x1 =>
            e1 >>= (fn e1 => return (x1, e1)))
          val branchR = withNewVar x2 (fn x2 =>
            e2 >>= (fn e2 => return (x2, e2)))
        in
          liftM4 Exp.Case' (tau, e, branchL, branchR)
        end


      type directive = directive


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
    | UnboundTypeVarError t =>
        Err ("Invalid type variable " ^ t ^ " (use A, B, C)")
    | ParseError error =>
        Err
          (case error of
             EOF => "Parse error: end of file"
           | Pos (_, pos) => "Parse error at " ^ Lexer.posToString pos)

  val parse =
    Stream.map (handler (ParserState.outState o #1 o ParseMain.parse))
    o splitBySemicolon o Lexer.lex
end

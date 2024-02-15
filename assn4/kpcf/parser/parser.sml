structure Parser :> PARSER =
struct
  datatype position = EOF | Pos of int * int

  exception ParseError of position * string option * string (*= Lexer.Error*)
  exception UnknownIdentifierError of string
  exception IncorrectSortError

  open ParserState
  structure MU = MonadUtil(ParserState)
  open MU
  infix 4 >>=
  infix 4 >>

  open KPCF

  type pos = int * int

  structure Arg =
  struct
    type string = string
    type int = int
    type typ = Typ.t monad
    (* type eprod = (Typ.t list) monad *)
    (* type lprod = (Typ.t list) monad *)
    (* type etup = (Value.t list) monad *)
    (* type ltup = (Exp.t list) monad *)
    (* type seq = (Value.t list) monad *)
    type exp = Exp.t monad
    type vars = string list

    fun id x = x

    val typeid = id
    (* val typlprod = liftM Typ.Lprod' *)
    (* val typeprod = liftM Typ.Eprod' *)
    fun typenat () = return Typ.Nat'
    fun typeunit () = return Typ.Unit'
    fun typevoid () = return Typ.Void'
    fun typeA () = return Typ.A'
    fun typeB () = return Typ.B'
    fun typeC () = return Typ.C'
    fun typeD () = return Typ.D'

    (* val typeseq = liftM Typ.Seq' *)
    (* val typegen = liftM Typ.Gen' *)
    val typecont = liftM Typ.Cont'
    val typefun = liftM2 Typ.Arrow'
    val typetimes = liftM2 Typ.Prod'
    val typesum = liftM2 Typ.Sum'

    val exp_id = id
    fun exp_var id =
      lookup id >>= (fn v => return (Exp.Var' v))
    (*
      (fn symbol =>
            case symbol of
              (v, Environment.FreeTermVariable) =>
                getEnv >>= (fn env =>
                  case Context.find (Environment.termContext env) v of
                    SOME (term, typ) => return term
                  | NONE => raise UnknownIdentifierError ("Undefined identifier: " ^ id))
            | (v, Environment.BoundTermVariable) => return (Exp.Var' v)
            | _ => raise UnknownIdentifierError ("Undefined identifier: " ^ id))
    *)

    (*
    tp >>= (fn typ =>
    withNewVar id Environment.BoundTermVariable
    (fn var =>
      tm >>= (fn t =>
      return (Exp.Lam' ((var, typ), t))))) *)

    fun exp_fun (fid, xid, tp1, tp2, em) =
      tp1
      >>=
      (fn typ1 =>
         tp2
         >>=
         (fn typ2 =>
            withNewVar fid (fn fvar =>
              withNewVar xid (fn xvar =>
                em
                >>=
                (fn e => return (Exp.Fun' ((typ1, typ2), ((fvar, xvar), e))))))))

    fun exp_lam (id, tp, em) =
      tp
      >>=
      (fn tp =>
         withNewVar id (fn id =>
           em >>= (fn e => return (Exp.Lam' ((id, tp), e)))))

    fun exp_z () = return Exp.Zero'
    val exp_succ = liftM Exp.Succ'

    fun exp_unit () = return Exp.Unit'

    val exp_prod = liftM2 Exp.Tuple'
    val exp_injl = liftM3 (fn (t1', t2', e') => Exp.InjL' ((t1', t2'), e'))
    val exp_injr = liftM3 (fn (t1', t2', e') => Exp.InjR' ((t1', t2'), e'))

    val exp_app = liftM2 Exp.Ap'

    val exp_throw = liftM3 Exp.Throw'

    val exp_abort = liftM2 Exp.Abort'

    fun exp_ifz (e, e0m, id, e1m) =
      liftM3 Exp.Ifz' (e, e0m, withNewVar id (fn var =>
        e1m >>= (fn e1 => return (var, e1))))

    fun exp_case (vm, id1, elm, id2, erm) =
      let
        val branchL = withNewVar id1 (fn x1 =>
          elm >>= (fn e1 => return (x1, e1)))
        val branchR = withNewVar id2 (fn x2 =>
          erm >>= (fn e2 => return (x2, e2)))
      in
        liftM3 Exp.Case' (vm, branchL, branchR)
      end

    fun exp_split (vm, id1, id2, em) =
      withNewVar id1 (fn x1 =>
        withNewVar id2 (fn x2 =>
          em
          >>= (fn e => vm >>= (fn v => return (Exp.Split' (v, ((x1, x2), e)))))))

    fun exp_letcc (tp, xid, em) =
      tp
      >>=
      (fn typ =>
         withNewVar xid (fn x =>
           em >>= (fn e => return (Exp.Letcc' (typ, (x, e))))))

    fun exp_let (id, e1, e2) =
      e1
      >>=
      (fn e1' =>
         withNewVar id (fn v =>
           e2 >>= (fn e2' => return (Exp.Let' (e1', (v, e2'))))))

    datatype terminal = datatype Token.token

    local open Token
    in
      fun tok_to_string tok =
        case tok of
          IDENT s => SOME ("IDENT " ^ s)

        | LPAREN => SOME "LPAREN"
        | RPAREN => SOME "RPAREN"
        | LBRACK => SOME "LBRACK"
        | RBRACK => SOME "RBRACK"
        | LBRACE => SOME "LBRACE"
        | RBRACE => SOME "RBRACE"
        | LANGLE => SOME "LANGLE"
        | RANGLE => SOME "RANGLE"

        | COMMA => SOME "COMMA"
        | BAR => SOME "BAR"
        | DOT => SOME "DOT"
        | COLON => SOME "COLON"
        | DARROW => SOME "DARROW"

        | NAT => SOME "NAT"
        | ARROW => SOME "ARROW"
        | UNIT => SOME "UNIT"
        | VOID => SOME "VOID"
        | TYPEA => SOME "A"
        | TYPEB => SOME "B"
        | TYPEC => SOME "C"
        | TYPED => SOME "D"
        | PLUS => SOME "PLUS"
        | TIMES => SOME "TIMES"

        | LLABEL => SOME "LLABEL"
        | RLABEL => SOME "RLABEL"

        | IFZ => SOME "IFZ"
        | CASE => SOME "CASE"
        | Z => SOME "Z"
        | S => SOME "S"
        | RET => SOME "RET"
        | LET => SOME "LET"
        | LETCC => SOME "LETCC"
        | CONT => SOME "CONT"
        | IS => SOME "IS"
        | IN => SOME "IN"
        | COMP => SOME "COMP"
        | THROW => SOME "THROW"
        | FN => SOME "FN"
        | FUN => SOME "FUN"
        | SPLIT => SOME "SPLIT"
    end

    fun error s =
      case Stream.front s of
        Stream.Nil => ParseError (EOF, NONE, "Parse error: end of file")
      | Stream.Cons ((tok, pos), _) =>
          ParseError (Pos pos, tok_to_string tok, "Parse error")
  end

  structure StreamWithPos =
    CoercedStreamable
      (structure Streamable = StreamStreamable
       type 'a item = 'a * pos
       fun coerce (x, _) = x)

  structure ParseMain =
    ParseMainFun (structure Streamable = StreamWithPos structure Arg = Arg)

  fun parse s =
    #2 (runState (StringRedBlackDict.empty) (#1
      (ParseMain.parse (Lexer.lex s))))

  fun parseFile fname =
    let
      val ins = TextIO.openIn fname
      val e = parse (Stream.fromTextInstream ins)
      val () = TextIO.closeIn ins
    in
      e
    end

end

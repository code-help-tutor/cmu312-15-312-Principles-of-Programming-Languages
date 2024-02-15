functor Parser(TopLevelCommands: TOP_LEVEL_COMMANDS) :>
  PARSER_CA
  where type cmd = TopLevelCommands.cmd =
struct
  datatype position = EOF | Pos of int

  exception ParseError of position (*= Lexer.Error*)
  exception UnknownIdentifierError of string

  type cmd = TopLevelCommands.cmd

  open ParserState
  structure MO = MonadOps(ParserState)
  open MO
  infix 4 >>=
  infix 4 >>

  structure Typ = CA.Typ;
  structure Exp = CA.Exp;
  structure Cmd = CA.Cmd;
  structure Chan = CA.Chan;

  infix 5 \
  infix 5 \\
  infix 5 !
  infix 5 !!

  type pos = int

  fun identity x = x
  fun lift x () = x
  fun null () = []

  fun retOp opr = return opr
  fun opM opr =
    liftM (fn e => opr e)
  fun opM2 opr =
    liftM2 (fn (e1, e2) => opr (e1, e2))
  fun opM3 opr =
    liftM3 (fn (e1, e2, e3) => opr (e1, e2, e3))
  val opsymM = opM
  val opsymM1 = opM2
  val opTpM = liftM2

  fun binop b (x, y) = Exp.Binop' (b, x, y)

  structure Arg =
  struct
    type string = string
    type int = int
    type variable = Exp.Var.t monad
    type symbol = Chan.t monad
    type channel = symbol
    type varterm = Exp.Var.t * Exp.t
    type directive' = TopLevelCommands.cmd
    type directive = directive' monad
    type term = Exp.t monad
    type binder = string * term
    type typ = Typ.t monad
    type fnarg = Exp.Var.t monad
    type funarg' = (string * Typ.t)
    type funarg = funarg' monad
    type funargs = (funarg' list) monad
    type fundecl' = varterm

    type fundecl = fundecl' monad
    type ident = (Exp.Var.t) monad

    type decl' = varterm
    type decl = decl' monad
    type decls = (decl' monad) list

    type cmd = Cmd.t monad

    val load = return o TopLevelCommands.Load

    fun termvar id =
      (getIdent id)
      >>=
      (fn ident =>
         case ident of
           Environment.FreeTermVariable v =>
             getEnv
             >>=
             (fn env =>
                case VariableContext.find (Environment.termContext env) v of
                  SOME (term, typ) => return term
                | NONE =>
                    raise UnknownIdentifierError ("Undefined identifier: " ^ id))
         | Environment.BoundTermVariable v => return (Exp.Var' v)
         | _ => raise UnknownIdentifierError ("Undefined identifier: " ^ id))

    fun termtriv () = return (Exp.Triv')

    fun absterm (id, tm) = (id, tm)

    val termid = identity
    val cmdid = identity

    fun termfn (id, typm, termm) =
      withNewVar id Environment.BoundTermVariable (fn var =>
        typm
        >>=
        (fn typ => termm >>= (fn term => return (Exp.Lam' (typ, (var, term))))))

    val termapp = opM2 Exp.App'
    val termpair = opM2 Exp.Pair'
    fun exp_split (vm, id1, id2, em) =
      withNewVar id1 Environment.BoundTermVariable (fn x1 =>
        withNewVar id2 Environment.BoundTermVariable (fn x2 =>
          em
          >>= (fn e => vm >>= (fn v => return (Exp.Split' (v, ((x1, x2), e)))))))

    val terminl = opTpM Exp.Inl'
    val terminr = opTpM Exp.Inr'

    val cmdret = opM Cmd.Ret'

    fun cmdbind (id, em, mm) =
      em
      >>=
      (fn e =>
         withNewVar id Environment.BoundTermVariable (fn var =>
           (mm >>= (fn m => return (Cmd.Bind' (e, (var, m)))))))

    fun mkchan id =
      (getIdent id)
      >>=
      (fn Environment.BoundAssignable sym => return sym
        | _ => raise UnknownIdentifierError ("Assignable expected: " ^ id))

    val cmdlist = liftM2 (fn (e1, e2) => let val var = Exp.Var.new "_"
                                         in Cmd.Bind' (Exp.Cmd' e1, (var, e2))
                                         end)

    val cmddo = liftM (fn (e1) =>
      let val var = Exp.Var.new "x"
      in Cmd.Bind' (e1, (var, Cmd.Ret' (Exp.Var' var)))
      end)

    val cmdprint = opM Cmd.Print'
    val cmdspawn = opM Cmd.Spawn'
    val cmdemitnoref = liftM2 (fn (c1, e2) => Cmd.Emit' (Exp.ChnRef' c1, e2))
    val cmdemit = opsymM1 Cmd.Emit'
    val cmdsyncnoref = liftM (fn c => Cmd.Sync' (Exp.ChnRef' c))
    val cmdsync = opM Cmd.Sync'
    fun cmdnewchan (id, tpm, cmdm) =
      tpm
      >>=
      (fn tp =>
         withNewSym id Environment.BoundAssignable (fn a =>
           cmdm >>= (fn m => return (Cmd.NewChn' (tp, (a, m))))))

    val termcmd = opM Exp.Cmd'
    val termchan = opsymM Exp.ChnRef'
    val termfold = opTpM Exp.Fold'
    val termunfold = opM Exp.Unfold'
    fun termstring s =
      retOp (Exp.String' s)
    fun termnum n =
      retOp (Exp.Num' n)
    fun termtrue () =
      retOp (Exp.Bool' true)
    fun termfalse () =
      retOp (Exp.Bool' false)
    val termif = opM3 (fn (x, y, z) => Exp.If' (x, (y, z)))
    fun termzero () =
      retOp (Exp.Num' 0)
    val termsucc = opM Exp.Succ'
    val termabort = opTpM Exp.Abort'
    val termplus = opM2 (binop Oper.Plus)
    val termminus = opM2 (binop Oper.Minus)
    val termtimes = opM2 (binop Oper.Times)
    val termdiv = opM2 (binop Oper.Div)
    val termmod = opM2 (binop Oper.Mod)
    val termeq = opM2 (binop Oper.Eq)
    val termneq = opM2 (binop Oper.Neq)
    val termlt = opM2 (binop Oper.Lt)
    val termlte = opM2 (binop Oper.Lte)
    val termgt = opM2 (binop Oper.Gt)
    val termgte = opM2 (binop Oper.Gte)
    val termandand = opM2 (binop Oper.AndAnd)
    val termoror = opM2 (binop Oper.OrOr)
    val termnot = opM Exp.Negate'

    fun termifz (e, e0m, (id, e1m)) =
      opM3 Exp.Ifz'
        ( e
        , e0m
        , withNewVar id Environment.BoundTermVariable (fn var =>
            e1m >>= (fn e1 => return (var, e1)))
        )

    fun termcase (e, (id1, elm), (id2, erm)) =
      let
        val branchL = withNewVar id1 Environment.BoundTermVariable (fn x1 =>
          elm >>= (fn e1 => return (x1, e1)))
        val branchR = withNewVar id2 Environment.BoundTermVariable (fn x2 =>
          erm >>= (fn e2 => return (x2, e2)))
      in
        opM3 Exp.Case' (e, branchL, branchR)
      end

    fun termdef (id, em) =
      em >>= (fn e => return (TopLevelCommands.TermDef (Exp.Var.new id, e)))

    val cmddef = liftM (fn c => TopLevelCommands.Command c)
    val baretermdef = liftM (fn term =>
      TopLevelCommands.TermDef (Exp.Var.new "it", term))
    val directiveid = identity

    fun decl1 decl = [decl]
    fun decl2 (decl, decls) = decl :: decls

    fun termdcl (id, em) =
      em >>= (fn e => return (Exp.Var.new id, e))
    val fundcl = identity

    fun funargs1 (id, typm) =
      typm >>= (fn typ => return [(id, typ)])

    fun funargs2 (id, typm, m) =
      typm >>= (fn typ => m >>= (fn args => return ((id, typ) :: args)))

    fun fundeclasc (id, argsm, typm, em) =
      let
        fun bindVar id typ em =
          withNewVar id Environment.BoundTermVariable (fn var =>
            liftM (fn e => Exp.Lam' (typ, (var, e))) em)
        fun wrapargs ((id, typ), (em, restyp)) =
          let
            val funtype = Typ.Arr' (typ, restyp)
            val body = bindVar id typ em
          in
            (body, funtype)
          end
      in
        typm
        >>=
        (fn t2 =>
           argsm
           >>=
           (fn args =>
              withNewVar id Environment.BoundTermVariable (fn f =>
                let
                  val ((id1, t1), (body, typ)) =
                    case args of
                      nil => raise Fail "impossible"
                    | (id1, t1) :: ts =>
                        ((id1, t1), List.foldr wrapargs (em, t2) ts)
                in
                  withNewVar id1 Environment.BoundTermVariable (fn x =>
                    body
                    >>= (fn b => return (f, Exp.Fun' ((t1, typ), ((f, x), b)))))
                end)))
      end

    val fundef = liftM (fn f => TopLevelCommands.TermDef f)

    fun termlet (defms: decls, em: term) =
      let
        fun wraplet (defm: decl, em: term) : Exp.t monad =
          defm
          >>=
          (fn def =>
             let
               val (var, e1) = def
             in
               withVar (Environment.BoundTermVariable var)
                 (em >>= (fn e => return (Exp.Let' (e1, (var, e)))))
             end)
      in
        foldr wraplet em defms
      end

    fun identnew id =
      return (Exp.Var.new id)

    fun typedef (id, tm) =
      liftM (fn t => TopLevelCommands.TypeDef (Typ.Var.new id, t)) tm

    fun typenat () = return Typ.Nat'
    fun typebool () = return Typ.Bool'
    fun typeunit () = return Typ.Unit'
    fun typevoid () = return Typ.Void'
    val typecmd = liftM Typ.Cmd'
    val typechn = liftM Typ.Chan'
    val typefun = liftM2 Typ.Arr'
    val typesum = liftM2 Typ.Sum'
    val typeprod = liftM2 Typ.Star'

    fun typevar id =
      (getIdent id)
      >>=
      (fn symbol =>
         case symbol of
           Environment.FreeTypeVariable v =>
             getEnv
             >>=
             (fn env =>
                case TypVariableContext.find (Environment.typeContext env) v of
                  SOME typ => return typ
                | NONE =>
                    raise UnknownIdentifierError ("Undefined identifier: " ^ id))
         | Environment.BoundTypeVariable v => return (Typ.Var' v)
         | _ => raise UnknownIdentifierError ("Undefined identifier: " ^ id))

    fun typerec (id, m) =
      withNewTypVar id Environment.BoundTypeVariable (fn t =>
        m >>= (fn tau => return (Typ.Rec' (t, tau))))

    val typeid = identity

    datatype terminal = datatype Token.token

    fun error s =
      case Stream.front s of
        Stream.Nil => ParseError EOF
      | Stream.Cons ((_, pos), _) => ParseError (Pos pos)
  end

  structure StreamWithPos =
    CoercedStreamable
      (structure Streamable = StreamStreamable
       type 'a item = 'a * pos
       fun coerce (x, _) = x)

  structure ParseMain =
    ParseMainFun (structure Streamable = StreamWithPos structure Arg = Arg)

  fun parse env s =
    #2 (runState (env, (Environment.identTable env)) (#1
      (ParseMain.parse (Lexer.lex s))))
end

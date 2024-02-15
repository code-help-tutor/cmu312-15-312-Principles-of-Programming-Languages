signature PARSER_STATE =
sig
  include STATE_MONAD
  exception UnknownIdError of string

  val withVar: Environment.ident -> 'a monad -> 'a monad
  val withNewVar: string
                  -> (CA.Exp.Var.t -> Environment.ident)
                  -> (CA.Exp.Var.t -> 'a monad)
                  -> 'a monad
  val withNewTypVar: string
                     -> (CA.Typ.Var.t -> Environment.ident)
                     -> (CA.Typ.Var.t -> 'a monad)
                     -> 'a monad
  val withNewSym: string
                  -> (CA.Chan.t -> Environment.ident)
                  -> (CA.Chan.t -> 'a monad)
                  -> 'a monad
  val pushIdent: Environment.ident -> unit monad
  val getIdent: string -> Environment.ident monad
  val getEnv: Environment.env monad
end

structure ParserState :>
  PARSER_STATE
  where type state = (Environment.env * (Environment.ident Symbols.dict)) =
struct
  structure STMonad =
    StateMonad(type s = (Environment.env * (Environment.ident Symbols.dict)))
  open STMonad

  structure MO = MonadOps(STMonad)
  open MO
  exception UnknownIdError of string

  infix 4 >>=
  infix 4 >>

  fun withVar ident m =
    get
    >>=
    (fn (e, st) =>
       put (e, Symbols.insert st (Environment.toString ident) ident) >> m
       >>= (fn a => put (e, st) >> return a))

  fun withNewVar id typ m =
    let val var = CA.Exp.Var.new id
    in withVar (typ var) (m var)
    end

  fun withNewTypVar id typ m =
    let val var = CA.Typ.Var.new id
    in withVar (typ var) (m var)
    end

  fun withNewSym id typ m =
    let val sym = CA.Chan.new id
    in withVar (typ sym) (m sym)
    end

  fun pushIdent ident =
    modifyState (fn (e, st) =>
      let val st' = Symbols.insert st (Environment.toString ident) ident
      in (e, st')
      end)

  fun getIdent id =
    get
    >>=
    (fn (e, st) =>
       case Symbols.find st id of
         SOME v => return v
       | _ => raise UnknownIdError ("Undefined identifier: " ^ id))

  val getEnv = get >>= (fn (e, st) => return e)
end

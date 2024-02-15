signature PARSER_STATE =
sig
  include STATE_MONAD
  exception UnknownIdError of string

  val withVar : Environment.ident -> 'a monad -> 'a monad
  val withNewVar : string -> (MA.Exp.Var.t -> Environment.ident) ->
    (MA.Exp.Var.t -> 'a monad) -> 'a monad
  val withNewTypVar : string -> (MA.Typ.Var.t -> Environment.ident) ->
    (MA.Typ.Var.t -> 'a monad) -> 'a monad
  val withNewSym : string -> (MA.Ref.t -> Environment.ident) ->
    (MA.Ref.t -> 'a monad) -> 'a monad
  val pushIdent : Environment.ident -> unit monad
  val getVar  : string -> Environment.ident monad
  val getSym  : string -> Environment.ident monad
  val getTVar : string -> Environment.ident monad
  val getEnv : Environment.env monad
end

structure ParserState :> PARSER_STATE
  where type state = (Environment.env * (Environment.ident Symbols.dict)) =
struct
  structure STMonad = StateMonad (type s = (Environment.env * (Environment.ident Symbols.dict)))
  open STMonad

  structure MO = MonadOps (STMonad)
  open MO
  exception UnknownIdError of string

  infix 4 >>=
  infix 4 >>

  fun withVar ident m =
    get >>= (fn (e, st) =>
    put (e, Symbols.insert st (Environment.toString ident) ident) >>
    m >>= (fn a =>
    put (e, st) >>
    return a))

  fun withNewVar id typ m =
  let
    val var = MA.Exp.Var.new ("$" ^ id)
  in
    withVar (typ var) (m var)
  end

  fun withNewTypVar id typ m =
  let
    val var = MA.Typ.Var.new ("@" ^ id)
  in
    withVar (typ var) (m var)
  end

  fun withNewSym id typ m =
    let
      val sym = MA.Ref.new ("&" ^ id)
    in
      withVar (typ sym) (m sym)
    end

  fun pushIdent ident =
    modifyState (fn (e, st) =>
      let
        val st' = Symbols.insert st (Environment.toString ident) ident
      in
        (e, st')
      end
    )

  fun getIdent id =
    get >>= (fn (e, st) =>
      case Symbols.find st id of
         SOME v => return v
       | _ => raise UnknownIdError ("Undefined identifier: " ^ id)
    )

  fun getVar id  = getIdent ("$" ^ id)
  fun getTVar id = getIdent ("@" ^ id)
  fun getSym id  = getIdent ("&" ^ id)

  val getEnv =
    get >>= (fn (e, st) => return e)
end

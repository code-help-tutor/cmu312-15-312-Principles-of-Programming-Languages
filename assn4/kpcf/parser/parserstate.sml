signature PARSER_STATE =
sig
  include STATE_MONAD
  exception UnknownIdError of string

  (* val withVar : Variable.t -> Environment.symtype -> 'a monad -> 'a monad
  val withNewVar : string -> Environment.symtype -> (Variable.t -> 'a monad) -> 'a monad
  val pushSymbol : Variable.t -> Environment.symtype -> unit monad
  val getSymbol : string -> (Variable.t * Environment.symtype) monad
  val getEnv : Environment.env monad *)
  val withNewVar: string -> (Variable.t -> 'a monad) -> 'a monad
  val lookup: string -> Variable.t monad
end

structure ParserState :> PARSER_STATE
                         where type state = Variable.t StringRedBlackDict.dict =
struct
  type state = Variable.t StringRedBlackDict.dict
  structure STMonad = StateMonad(type s = Variable.t StringRedBlackDict.dict)
  open STMonad

  structure MU = MonadUtil(STMonad)
  open MU
  exception UnknownIdError of string

  infix 4 >>=
  infix 4 >>

  (*
  fun withVar var typ m =
    get >>= (fn (e, st) =>
    put (e, Symbols.insert st (Variable.toUserString var) (var, typ)) >>
    m >>= (fn a =>
    put (e, st) >>
    return a))

  fun withNewVar id typ m =
    let
      val var = Variable.new id
    in
      withVar var typ (m var)
    end

  fun pushSymbol var typ =
    modifyState (fn (e, st) =>
      let
        val st' = Symbols.insert st (Variable.toUserString var) (var, typ)
      in
        (e, st')
      end
    )

  fun getSymbol id =
    get >>= (fn (e, st) =>
      case Symbols.find st id of
         SOME v => return v
       | _ => raise UnknownIdError ("Undefined identifier: " ^ id)
    )

  val getEnv =
    get >>= (fn (e, st) => return e) *)

  fun withNewVar s m =
    let
      val new_var = Variable.new s
    in
      get
      >>=
      (fn dict =>
         put (StringRedBlackDict.insert dict s new_var) >> m new_var
         >>= (fn a => put dict >> return a))
    end

  fun lookup s =
    get
    >>=
    (fn state =>
       case StringRedBlackDict.find state s of
         SOME v => return v
       | NONE => raise UnknownIdError s)
end

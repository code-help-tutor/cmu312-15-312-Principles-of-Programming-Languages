signature PARSER_STATE =
  sig
    structure Var : TEMP

    include STATE_MONAD
    exception UnknownIdError of string

    val withNewVar : string -> (Var.t -> 'a monad) -> 'a monad
    val lookup : string -> Var.t monad

    val outState : 'a monad -> 'a
  end

functor ParserState (Var : TEMP) :> PARSER_STATE where Var = Var =
  struct
    structure Var = Var
    structure Dict = StringRedBlackDict

    structure STMonad = StateMonad (type s = Var.t StringSplayDict.dict)
    open STMonad

    structure MU = MonadUtil (STMonad)
    open MU
    exception UnknownIdError of string

    infix 4 >>=
    infix 4 >>

    fun withNewVar s m =
      let
        val new_var = Var.new s
      in
        get >>= (fn dict =>
        put (StringSplayDict.insert dict s new_var) >>
        m new_var >>= (fn a =>
        put dict >>
        return a))
      end

    fun lookup s =
      get >>= (fn state =>
        case StringSplayDict.find state s of
          SOME v => return v
        | NONE => raise UnknownIdError s
      )

    fun outState s = #2 (runState StringSplayDict.empty s)
  end

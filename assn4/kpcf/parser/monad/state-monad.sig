signature STATE_MONAD =
sig
  include MONAD

  type state

  val get: state monad
  val put: state -> unit monad
  val modifyState: (state -> state) -> unit monad
  val runState: state -> 'a monad -> (state * 'a)
end

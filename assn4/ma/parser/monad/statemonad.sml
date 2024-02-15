signature STATE_MONAD =
sig
  include MONAD

  type state

  val get : state monad
  val put : state -> unit monad
  val modifyState : (state -> state)-> unit monad
  val runState : state -> 'a monad -> (state * 'a)
end

functor StateMonad (type s) :> STATE_MONAD where type state = s =
struct
  type state = s
  datatype 'a STMonad =
    ST of state -> (state *'a)

  type 'a monad = 'a STMonad

  fun return a = ST (fn s => (s, a))

  fun >>= (ST am,  f) =
    ST (fn table =>
           let
             val (table', a) = am table
             val (ST bm) = f a
           in
             bm table'
           end)

  val get = ST (fn s => (s, s))

  fun put s = ST (fn _ => (s, ()))

  fun modifyState f = ST (fn s => (f s, ()))

  fun runState st (ST am) = am st
end


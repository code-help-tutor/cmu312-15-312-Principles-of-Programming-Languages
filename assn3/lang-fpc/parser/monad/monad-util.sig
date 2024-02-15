signature MONAD_UTIL =
sig
  include MONAD

  val =<< : ('a -> 'b monad) -> 'a monad -> 'b monad
  val >> : ('a monad * 'b monad) -> 'b monad

  val foldM: ('a * 'b -> 'b monad) -> 'b -> 'a list -> 'b monad

  val liftM: ('a -> 'b) -> 'a monad -> 'b monad

  val liftM0: 'a -> unit -> 'a monad
  val liftM1: ('a -> 'b) -> 'a monad -> 'b monad
  val liftM2: (('a * 'b) -> 'c) -> ('a monad * 'b monad) -> 'c monad
  val liftM3: (('a * 'b * 'c) -> 'd)
              -> ('a monad * 'b monad * 'c monad)
              -> 'd monad
  val liftM4: (('a * 'b * 'c * 'd) -> 'e)
              -> ('a monad * 'b monad * 'c monad * 'd monad)
              -> 'e monad
end

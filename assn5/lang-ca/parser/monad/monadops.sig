signature MONAD_OPS =
sig
  include MONAD

  val =<< : ('a -> 'b monad) -> 'a monad -> 'b monad
  val >> : ('a monad * 'b monad) -> 'b monad
  val bindpair: ('a monad * 'b monad) -> (('a * 'b) -> 'c monad) -> 'c monad

  val foldM: ('a * 'b -> 'b monad) -> 'b -> 'a list -> 'b monad

  val liftM: ('a -> 'b) -> 'a monad -> 'b monad
  val liftM2: (('a * 'b) -> 'c) -> ('a monad * 'b monad) -> 'c monad
  val liftM3: (('a * 'b * 'c) -> 'd)
              -> ('a monad * 'b monad * 'c monad)
              -> 'd monad
end

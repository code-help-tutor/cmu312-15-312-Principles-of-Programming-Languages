signature MONAD =
sig
  type 'a monad
  val return: 'a -> 'a monad
  val >>= : ('a monad * ('a -> 'b monad)) -> 'b monad
end

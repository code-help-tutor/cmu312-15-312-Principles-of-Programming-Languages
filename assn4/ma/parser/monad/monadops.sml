functor MonadOps (M : MONAD) : MONAD_OPS =
struct
  open M
  infix 4 >>=

  fun =<< f m = m >>= f

  fun >> (a, b) = a >>= (fn _ => b)

  fun bindpair (am, bm) f =
    am >>= (fn a =>
    bm >>= (fn b => f (a, b)))

  fun foldM _ b [] = return b
    | foldM f b (x :: xs) =
        f (x, b) >>= (fn fxb => foldM f fxb xs)


  fun liftM f am =
    am >>= (return o f)

  fun liftM2 f m =
    bindpair m (return o f)

  fun liftM3 f (m1, m2, m3) =
    m1 >>= (fn a =>
    m2 >>= (fn b =>
    m3 >>= (fn c =>
    return (f (a,b,c)))))
end

functor MonadUtil(M: MONAD): MONAD_UTIL =
struct
  open M
  infix 4 >>=

  fun =<< f m = m >>= f

  fun >> (a, b) =
    a >>= (fn _ => b)

  fun foldM _ b [] = return b
    | foldM f b (x :: xs) =
        f (x, b) >>= (fn fxb => foldM f fxb xs)


  fun liftM f am =
    am >>= (return o f)


  fun liftM0 f () = return f

  val liftM1 = liftM

  fun liftM2 f (m1, m2) =
    m1 >>= (fn a => m2 >>= (fn b => return (f (a, b))))

  fun liftM3 f (m1, m2, m3) =
    m1 >>= (fn a => m2 >>= (fn b => m3 >>= (fn c => return (f (a, b, c)))))

  fun liftM4 f (m1, m2, m3, m4) =
    m1
    >>=
    (fn a =>
       m2
       >>= (fn b => m3 >>= (fn c => m4 >>= (fn d => return (f (a, b, c, d))))))
end

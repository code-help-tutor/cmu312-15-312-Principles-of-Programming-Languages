structure NatOp =
struct
  datatype 't view = Zero | Succ of 't

  val map = fn (f: 't1 -> 't2) => fn Zero => Zero | Succ t => Succ (f t)
end

signature LIST_OP =
sig
  type element

  datatype 't view = Nil | Cons of element * 't
  val map: ('rho1 -> 'rho2) -> 'rho1 view -> 'rho2 view
end

functor ListOp (type element) :> LIST_OP where type element = element =
struct
  type element = element

  datatype 't view = Nil | Cons of element * 't

  val map = fn (f: 'rho1 -> 'rho2) =>
    fn Nil => Nil | Cons (e, x) => Cons (e, f x)
end

structure AutomatonOp =
struct
  type 't view = bool * (char -> 't)

  val map = fn f => fn (b, d) => (b, f o d)
end

signature STATE =
sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t

  val initial: 'a -> 'a t
  val bind: 'a t * ('a -> 'b t) -> 'b t

  val toString: ('a -> string) -> 'a t -> string
end

signature QUEUE =
sig
  type 'a t

  val empty: 'a t
  val singleton: 'a -> 'a t
  val insert: 'a t -> 'a -> 'a t
  val front: 'a t -> ('a * 'a t) option

  val append: 'a t * 'a t -> 'a t

  val size: 'a t -> int

  val map: ('a -> 'b) -> 'a t -> 'b t
  val foldl: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val filter: ('a -> bool) -> 'a t -> 'a t

  val remove: ('a * 'a -> bool) -> 'a t -> 'a -> 'a t

  val toList: 'a t -> 'a list
  val toString: ('a -> string) -> 'a t -> string
end

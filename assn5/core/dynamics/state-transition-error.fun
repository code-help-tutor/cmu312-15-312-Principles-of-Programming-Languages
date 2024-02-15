functor TransitionErrorState(Value: SHOW) :>
sig
  datatype ('a, 'v) state = Step of 'a | Val of 'v | Err
  val map2: ('a -> 'b) * ('v -> 'w) -> ('a, 'v) state -> ('b, 'w) state

  include STATE where type 'a t = ('a, Value.t) state
end =
struct
  datatype ('a, 'v) state = Step of 'a | Val of 'v | Err

  fun map2 (f: 'a -> 'b, g: 'v -> 'w) : ('a, 'v) state -> ('b, 'w) state =
    fn Step a => Step (f a) | Val v => Val (g v) | Err => Err

  type 'a t = ('a, Value.t) state

  fun map (f: 'a -> 'b) : 'a t -> 'b t = map2 (f, Fn.id)

  val initial = Step

  fun bind (t: 'a t, f: 'a -> 'b t) : 'b t =
    case t of
      Step x => f x
    | Val v => Val v
    | Err => Err

  fun toString (f: 'a -> string) : 'a t -> string =
    fn Step x => "|--> " ^ f x
     | Val v => "val " ^ Value.toString v
     | Err => "err"
end

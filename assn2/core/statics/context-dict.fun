functor DictContext (structure Dict: DICT type codomain) :>
  CONTEXT
  where type context = codomain Dict.dict =
struct
  type context = codomain Dict.dict
  type t = context

  val empty = Dict.empty
  val append = fn (d1, d2) =>
    Dict.union d1 d2 (fn (_, _, v) => (* shadow to the right *) v)
end

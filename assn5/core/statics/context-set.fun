functor SetContext (structure Set: SET) :> CONTEXT where type context = Set.set =
struct
  type context = Set.set
  type t = context

  val empty = Set.empty
  val append = Fn.uncurry Set.union
end

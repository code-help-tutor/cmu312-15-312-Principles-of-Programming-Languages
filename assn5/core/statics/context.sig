signature CONTEXT =
sig
  type context
  type t = context

  (* `empty` and `append` should form a monoid. *)
  val empty: context
  val append: context * context -> context
end

functor ProductContext (structure Context1: CONTEXT structure Context2: CONTEXT) :>
  CONTEXT
  where type context = Context1.context * Context2.context =
struct
  type context = Context1.context * Context2.context
  type t = context

  val empty = (Context1.empty, Context2.empty)
  val append = fn ((ctx1, ctx2), (ctx1', ctx2')) =>
    (Context1.append (ctx1, ctx1'), Context2.append (ctx2, ctx2'))
end

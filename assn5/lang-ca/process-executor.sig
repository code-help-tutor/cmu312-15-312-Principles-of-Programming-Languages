signature EXECUTOR =
sig
  type context

  exception Malformed
  val run: context -> context
end

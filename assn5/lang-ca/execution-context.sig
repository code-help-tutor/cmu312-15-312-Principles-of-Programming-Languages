signature EXECUTION_CONTEXT =
sig
  include STATE

  type chan (* parameter *)
  type msg (* parameter *)

  (* Choose a ready process. *)
  val chooseProcess: 'a t -> 'a option * 'a t

  (* An empty context. *)
  val empty: 'a t

  (* Concurrently combine two contexts.
     You may assume the *bound* channels are disjoint, by alpha-variance.

     If a channel would have both an receiving process and a sent message,
     concurrent combination should provide the message to the process
     and consider it ready. If that waiter was also waiting on other channels,
     it should stop.
   *)
  val conc: 'a t * 'a t -> 'a t

  (* Create a context with a single message on a channel. *)
  val send: chan -> msg -> 'a t

  (* Create a context with a process pool waiting on a channel. *)
  val recv: chan -> (msg -> 'a t) -> 'a t

end

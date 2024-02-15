functor ExecutionContext
  (structure Chan:
   sig
     include ORDERED SHOW
   end
   structure Msg: SHOW
   structure Queue: QUEUE) :> EXECUTION_CONTEXT
                              where type chan = Chan.t
                              and type msg = Msg.t =
struct
  type chan = Chan.t
  and msg = Msg.t

  structure ChanDict = SplayDict (structure Key = Chan)

  datatype 'a t =
    State of
      (* the state (messages or waiters) associated with each known channel *)
      'a chanState ChanDict.dict
      (* the waiter associated with each process identifier *)
      (* a queue of ready processes *)
      * 'a Queue.t

  (* Associated with a channel is either:
    - a list of messages ready to send, or
    - a list of process id's for waiting processes
    No messages and no waiters are equivalent scenarios, akin to +0 and -0. *)
  and 'a chanState =
    Messages of msg Queue.t
  | Waiters of 'a waiter Queue.t
  (* invariant: Messages Queue.empty = Waiters Queue.empty *)

  withtype 'a waiter = msg -> 'a t

  exception Unimplemented

  fun chooseProcess (State (chans, ready) : 'a t) : 'a option * 'a t =
    raise Unimplemented

  val empty: 'a t = State (ChanDict.empty, Queue.empty)

  (* Concurrently combine two contexts. See the signature for the specification.

     Remark: perhaps unsurprisingly, this function is substantially more difficult to
     implement than any of the others (arguably, all of the others combined).
     You may wish to attempt it last so you can test using the other functions.
   *)
  fun conc ((State (chans, ready), State (chans', ready')): 'a t * 'a t) : 'a t =
    raise Unimplemented

  (* Singleton `ready` process. *)
  (* Create a context with a single ready-to-run process. *)
  fun initial (x: 'a) : 'a t = raise Unimplemented

  (* Singleton context, sending `c` along `m`. *)
  fun send (c: chan) (m: msg) : 'a t = raise Unimplemented

  (* Single waiter `w`, waiting on channel `c`. *)
  fun recv (c: chan) (w: 'a waiter) : 'a t = raise Unimplemented


  (* Auxiliary *)
  fun map (f: 'a -> 'b) (State (chans, ready) : 'a t) : 'b t =
    State
      ( ChanDict.map
          (fn Messages ms => Messages ms
            | Waiters ws => Waiters (Queue.map (fn w => map f o w) ws)) chans
      , Queue.map f ready
      )

  local
    fun join (State (chans, ready) : 'a t t) : 'a t =
      Queue.foldl conc
        (State
           ( ChanDict.map
               (fn Messages ms => Messages ms
                 | Waiters ws => Waiters (Queue.map (fn w => join o w) ws))
               chans
           , Queue.empty
           )) ready
  in
    fun bind (x, f) =
      join (map f x)
  end

  fun toString (f: 'a -> string) (State (chans, ready) : 'a t) : string =
    let
      fun chanStateToString (Messages es) =
            "    Messages:\n"
            ^
            String.concat
              (List.map (fn msg => "    " ^ Msg.toString msg ^ "\n")
                 (Queue.toList es))
        | chanStateToString (Waiters ws) =
            "    Waiters: " ^ Int.toString (Queue.size ws) ^ "\n"
      fun channelInfo (c, data) =
        "  " ^ Chan.toString c ^ "\n" ^ chanStateToString data

      fun procInfo p =
        "  " ^ f p ^ "\n"
    in
      "Channels:\n"
      ^ String.concat (List.map channelInfo (ChanDict.toList chans))
      ^ "Ready processes:\n"
      ^ String.concat (List.map procInfo (Queue.toList ready)) ^ "\n"
    end
end

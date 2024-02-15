structure StateMA :> sig  structure MemDict : DICT where type key = MA.Ref.t
                          type memory = MA.Exp.t MemDict.dict
                          datatype 'a state = Step of 'a | Final of MA.Exp.t
                          include STATE where type 'a t = memory -> ('a * memory) state
                     end =
struct

    structure Ref = struct
    type t = MA.Ref.t
    val toString = MA.Ref.toString
    val compare = MA.Ref.compare
    val eq = MA.Ref.equal
    end

    structure MemDict = SplayDict (structure Key = Ref)

    type memory = MA.Exp.t MemDict.dict

    datatype 'a state = Step of 'a | Final of MA.Exp.t

    type 'a t = memory -> ('a * memory) state

    fun map (f : 'a -> 'b) : 'a t -> 'b t =
        fn (t : 'a t) => fn mem =>
           case t mem of
               Step (exp', mem') => Step (f exp', mem')
            |  Final e => Final e

    fun initial (data : 'a) : 'a t =
        fn mem => Step (data, mem)

    fun bind (exp : 'a t, step : 'a -> 'b t) : 'b t = fn m =>
        case exp m of
            Step (exp', Memory') => (step exp') Memory'
          | Final e  => Final e

    fun toString (f : 'a -> string) : 'a t -> string =
        fn s => (
            case s (MemDict.empty) of
                Step (s', Memory') => "|--> " ^ f s'
              | Final e => MA.Exp.toString e
        )

end

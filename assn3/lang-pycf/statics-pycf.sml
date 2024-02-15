structure StaticsPyCF :>
  STATICS
    where type Typ.t  = unit
      and type Term.t = PyCF.Object.t
      and Context = ContextPyCF
  =
  let
    structure Object = PyCF.Object
  in
    struct
      structure Typ  =
        struct
          type t = unit
          val toString = fn () => "ok"
        end

      and       Term = Object

      structure Context = ContextPyCF

      structure Error = StaticsErrorPyCF
      exception TypeError of Error.t

      fun inferType (ctx : Context.t) (object : Object.t) : Typ.t =
        case Object.out object of
          Object.Var x =>
            if Context.find ctx x
              then ()
              else raise TypeError (Error.Var x)
        | Object.Bool _ => ()
        | Object.If (d, d1, d0) =>
            ( inferType ctx d
            ; inferType ctx d1
            ; inferType ctx d0
            )
        | Object.Int _ => ()
        | Object.Plus (d1, d2) =>
            ( inferType ctx d1
            ; inferType ctx d2
            )
        | Object.LEq (d1, d2) =>
            ( inferType ctx d1
            ; inferType ctx d2
            )
        | Object.List l => List.app (inferType ctx) l
        | Object.Index (d, d_index) =>
            ( inferType ctx d
            ; inferType ctx d_index
            )
        | Object.Len d => inferType ctx d
        | Object.Fun (f, (x, d)) =>
            inferType (Context.insert (Context.insert ctx f) x) d
        | Object.Ap (d, d1) =>
            ( inferType ctx d
            ; inferType ctx d1
            )
        | Object.Let (d1, (x, d2)) =>
            ( inferType ctx d1
            ; inferType (Context.insert ctx x) d2
            )
        | Object.IsInstance (d, class) => inferType ctx d

      and checkType (ctx : Context.t) (object : Object.t) (() : Typ.t) : unit =
        inferType ctx object
    end
  end

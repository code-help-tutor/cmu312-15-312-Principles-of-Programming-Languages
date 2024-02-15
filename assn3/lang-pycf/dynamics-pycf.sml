structure DynamicsPyCF :>
  DYNAMICS
    where State = StatePyCF
      and type term = PyCF.Object.t
  =
  let
    structure Object = PyCF.Object
    structure Class = PyCF.Class
  in
    struct
      structure State = StatePyCF

      type term = Object.t

      structure Error = DynamicsErrorPyCF
      exception Malformed of Error.t

      fun progress (object : Object.t) : Object.t State.t =
        case Object.out object of
          Object.Var x => raise Malformed (Error.Var x)
        | Object.Bool b => State.Val (Object.Bool' b)
        | Object.If (d, d1, d0) => (
            case progress d of
              State.Step d' => State.Step (Object.If' (d', d1, d0))
            | State.Val d => (
                case Object.out d of
                  Object.Bool b => State.Step (if b then d1 else d0)
                | _ => State.Err
              )
            | State.Err => State.Err
          )
        | Object.Int i => State.Val (Object.Int' i)
        | Object.Plus (d1, d2) => (
            case progress d1 of
              State.Step d1' => State.Step (Object.Plus' (d1', d2))
            | State.Val d1 => (
                case progress d2 of
                  State.Step d2' => State.Step (Object.Plus' (d1, d2'))
                | State.Val d2 => (
                    case (Object.out d1, Object.out d2) of
                      (Object.Int i1, Object.Int i2) => State.Step (Object.Int' (i1 + i2))
                    | (Object.List l1, Object.List l2) => State.Step (Object.List' (l1 @ l2))
                    | _ => State.Err
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Object.LEq (d1, d2) => (
            case progress d1 of
              State.Step d1' => State.Step (Object.LEq' (d1', d2))
            | State.Val d1 => (
                case progress d2 of
                  State.Step d2' => State.Step (Object.LEq' (d1, d2'))
                | State.Val d2 => (
                    case (Object.out d1, Object.out d2) of
                      (Object.Int i1, Object.Int i2) => State.Step (Object.Bool' (i1 <= i2))
                    | _ => State.Err
                  )
                | State.Err => State.Err
              )
            | State.Err => State.Err
          )
        | Object.List l =>
            let
              fun progressList nil       = State.Val nil
                | progressList (d :: ds) = (
                    case progress d of
                      State.Step d' => State.Step (d' :: ds)
                    | State.Val d =>
                        State.map2
                          (Fn.curry (op ::) d, Fn.curry (op ::) d)
                          (progressList ds)
                    | State.Err => State.Err
                  )
            in
              State.map2
                (Object.List', Object.List')
                (progressList l)
            end
        | Object.Index (d, d_index) => raise Fail "Unimplemented"
        | Object.Len d => (
            case progress d of
              State.Step d' => State.Step (Object.Len' d')
            | State.Val d => (
                case Object.out d of
                  Object.List l => State.Step (Object.Int' (List.length l))
                | _ => State.Err
              )
            | State.Err => State.Err
          )
        | Object.Fun _ => raise Fail "Unimplemented"
        | Object.Ap (d, d1) => raise Fail "Unimplemented"
        | Object.Let (d1, (x, d2)) => (
            case progress d1 of
              State.Step d1' => State.Step (Object.Let' (d1', (x, d2)))
            | State.Val d1 => State.Step (Object.subst d1 x d2)
            | State.Err => State.Err
          )
        | Object.IsInstance (d, class) => raise Fail "Unimplemented"
    end
  end

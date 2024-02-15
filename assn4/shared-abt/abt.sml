structure Abt :> ABT =
struct
  datatype 'oper t =
    FV of Variable.t
  | BV of int
  | ABS of string * 'oper t
  | OPER of 'oper

  datatype 'oper view =
    Var of Variable.t
  | Binding of Variable.t * 'oper t
  | Oper of 'oper

  type 'a binding_modifier = Variable.t -> int -> 'a -> 'a

  exception Malformed

  fun bind bind_oper x i t =
    case t of
      FV y => if Variable.equal (x, y) then BV i else FV y
    | ABS (name, t) => ABS (name, bind bind_oper x (i + 1) t)
    | BV n => BV n
    | OPER f => OPER (bind_oper x i f)

  fun unbind unbind_oper x i t =
    case t of
      BV j => if i = j then FV x else BV j
    | ABS (name, t) => ABS (name, unbind unbind_oper x (i + 1) t)
    | FV x => FV x
    | OPER f => OPER (unbind_oper x i f)

  fun into bind_oper v =
    case v of
      Var x => FV x
    | Binding (x, t) => ABS (Variable.toUserString x, bind bind_oper x 0 t)
    | Oper f => OPER f

  fun out unbind_oper t =
    case t of
      BV _ => raise Fail "Internal Abbot Error"
    | FV x => Var x
    | OPER f => Oper f
    | ABS (name, t) =>
        let val var = Variable.new name
        in Binding (var, unbind unbind_oper var 0 t)
        end

  fun aequiv oper_eq (t1, t2) =
    case (t1, t2) of
      (BV i, BV j) => i = j
    | (FV x, FV y) => Variable.equal (x, y)
    | (ABS (_, t1'), ABS (_, t2')) => aequiv oper_eq (t1', t2')
    | (OPER f1, OPER f2) => oper_eq (f1, f2)
    | _ => false
end

structure Oper = struct
datatype t = Plus
           | Minus
           | Times
           | Div
           | Mod
           | AndAnd
           | OrOr
           | Lt
           | Lte
           | Gt
           | Gte
           | Eq
           | Neq

val equal : t * t -> bool = op=
fun toString v =
    case v
      of Plus => "Plus"
       | Minus => "Minus"
       | Times => "Times"
       | Div => "Div"
       | Mod => "Mod"
       | AndAnd => "LAnd"
       | OrOr => "LOr"
       | Lt => "LessThan"
       | Lte => "LEQ"
       | Gt =>"GreaterThan"
       | Gte => "GEQ"
       | Eq => "Equals"
       | Neq => "NotEquals"
end

local
    structure String' = struct
    type t = string
    val equal : t * t -> bool = op=
    val toString : t -> string = fn s => "\"" ^ String.toString s ^ "\""
    end

    structure Int' = struct
    type t = int
    val equal : t * t -> bool = op=
    val toString : t -> string = Int.toString
    end

    structure Bool' = struct
    type t = bool
    val equal : t * t -> bool = op=
    val toString : t -> string = Bool.toString
    end
in
    structure MA = MA (
        structure String = String'
        structure Oper = Oper
        structure Int = Int'
        structure Bool = Bool'
    )
end

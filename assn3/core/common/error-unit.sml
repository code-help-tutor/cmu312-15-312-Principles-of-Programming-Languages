structure UnitError :> SHOW where type t = unit =
struct type t = unit val toString = fn () => "error" end

structure StringError :> SHOW where type t = string =
struct type t = string val toString = Fn.id end

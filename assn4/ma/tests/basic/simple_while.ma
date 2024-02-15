/* result: ~55 */

local r := 0 in
local a := 10 in {
while([a](a > 0)) {
  [r, a]{ set r := r - a },
  [a]   { set a := a - 1 }
},
get r
}
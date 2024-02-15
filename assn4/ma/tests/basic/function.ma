/* result: 1 */


fun f (x : nat) : nat =
  x + 1
;

local a := 0 in {
    cmdlet x = get a in
    ret (f (x))
}
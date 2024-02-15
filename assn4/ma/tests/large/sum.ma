/* result: 10000 */

/* sums from 0 to i */
fun sum (i : nat) : cmd[nat] = cmd {
  local r := 0 in
  local c := 0 in {
    while([c](c <= i)) {
        [r, c]{ set r := r + c },
        [c] { set c := c + 1 }
    },
    get r
  }
}

;

do (cmd { print('result from sum.ma:\n') });
bndcmdexp x = sum (100) in
bndcmdexp y = sum (99) in
  ret (x + y)
/* result: 4 */

fun f (n : nat) : cmd[nat] =
  cmd { ret(n * 2) }

;

do(f(2))
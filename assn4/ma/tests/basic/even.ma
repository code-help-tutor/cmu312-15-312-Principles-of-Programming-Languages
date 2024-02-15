/* result: true */

fun is_even (n : nat) : cmd[bool] =
    ifz n % 2
    then cmd { ret (true) }
    else n' => cmd { ret (false) }
;

bndcmdexp x = is_even 10 in
    ret (x)
/* result: 34 */

fun fib (n : nat) : cmd[nat] =
    ifz n
    then cmd { ret(0) }
    else n' =>
        ifz n'
        then cmd { ret(1) }
        else n'' => cmd {
            local i := 3 in
            local t1 := 0 in
            local t2 := 1 in
            local res := 1 in {
                while ([i] (i <= n) ) {
                    [t2] { set t1 := t2 },
                    [res] { set t2 := res },
                    [t1, t2] { set res := t1 + t2 },
                    [i] { set i := i + 1 }
                },
                get res
            }
        }

;

do(fib(9))
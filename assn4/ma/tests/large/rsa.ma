/* result: 42 */

fun dec (ciphertext : nat) (d : nat) (n : nat) : cmd[nat] =
    cmd {
        local res := ciphertext in
        local i := 1 in {
            while ([i] (i < d)) {
                [res] { set res := (res * ciphertext) % n },
                [i] { set i := i + 1 }
            },
            get res
        }
    }
;

bndcmdexp x = dec 66 47 391 in
    ret(x)
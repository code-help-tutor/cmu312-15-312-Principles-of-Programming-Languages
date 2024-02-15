
fun factor (n : nat) (try : nat) : cmd[nat] = cmd {
    if ret(n = 1)
    then {
        ret (0)
    }
    else {
        if (ret(n % try = 0))
        then {
            ret (try)
        }
        else {
            bndcmdexp r = factor (n) (try + 1) in
                ret (r)
        }
    }
}

;

/* this is buggy, it doesn't do what I want it to do */
fun totient (n : nat) : cmd[nat] = cmd {
    local numer := 1 in
    local denom := 1 in
    local p := 2 in
    local n' := n in
    local orig_n := n in {
        while ([n'] (n' != 1)) {
            [n', p] {
                bndcmdexp r = factor (n') (p) in {
                    set p := r
                }
            },
            [numer, p] { set numer := numer * (p - 1) },
            [denom, p] { set denom := denom * p },
            while ([n', p] (n' % p = 0)) {
                [n', p] { set n' := n' / p }
            }
        },
        [orig_n, denom, numer] { ret( (orig_n / denom) / numer ) }
    }
}

;

local sum := 0 in
local i := 1 in {
    while ([i] (i < 100)) {
        [i] {
            bndcmdexp r = totient (i) in {
                [sum] { set sum := sum + r }
            }
        },
        [i] { set i := i + 1 }
    },
    [sum] { ret(sum) }
}
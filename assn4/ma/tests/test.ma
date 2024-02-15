load "tasks/gcd.ma";
load "tasks/collatz.ma";
load "large/sum.ma";

fun test1p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 10 15 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 5))
                                then {
                                    print('test1 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test1 failed\n')
                        }
                | r => cmd { print('test1 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test2p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 12 8 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 4))
                                then {
                                    print('test2 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test2 failed\n')
                        }
                | r => cmd { print('test2 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test3p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 27 18 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 9))
                                then {
                                    print('test3 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test3 failed\n')
                        }
                | r => cmd { print('test3 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test4p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 35 49 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 7))
                                then {
                                    print('test4 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test4 failed\n')
                        }
                | r => cmd { print('test4 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test5p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 144 288 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 144))
                                then {
                                    print('test5 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test5 failed\n')
                        }
                | r => cmd { print('test5 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test6p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 17 23 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 1))
                                then {
                                    print('test6 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test6 failed\n')
                        }
                | r => cmd { print('test6 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test7p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 0 5 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 5))
                                then {
                                    print('test7 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test7 failed\n')
                        }
                | r => cmd { print('test7 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test8p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 123456789 987654321 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { if (ret(l = 9))
                                then {
                                    print('test8 passed\n'),
                                    [count] { set count := 1 }
                                }
                                else print('test8 failed\n')
                        }
                | r => cmd { print('test8 failed\n') }
            }
        ) in {
            get count
        }
    }
}

;

fun test9p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = gcd 0 0 in {
        bndcmdexp c = (
            case res of {
                  l => cmd { print('test9 failed\n') }
                | r => cmd { print('test9 passed\n') , [count] { set count := 1 } }
            }
        ) in {
            get count
        }
    }
}

;

fun test10p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = collatz 10 in {
        if (ret(res = 6))
            then { print('test10 passed\n'), [count] { set count := 1 } , get count }
            else { print('test10 failed\n'), get count }
    }
}

;

fun test11p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = collatz 20 in {
        if (ret(res = 7))
            then { print('test11 passed\n'), [count] { set count := 1 } , get count }
            else { print('test11 failed\n'), get count }
    }
}

;

fun test12p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = collatz 7 in {
        if (ret(res = 16))
            then { print('test12 passed\n'), [count] { set count := 1 } , get count }
            else { print('test12 failed\n'), get count }
    }
}

;

fun test13p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = collatz 1 in {
        if (ret(res = 0))
            then { print('test13 passed\n'), [count] { set count := 1 } , get count }
            else { print('test13 failed\n'), get count }
    }
}

;

fun test14p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    bndcmdexp res = collatz 27 in {
        if (ret(res = 111))
            then { print('test14 passed\n'), [count] { set count := 1 } , get count }
            else { print('test14 failed\n'), get count }
    }
}

;

fun test15p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    cmdlet x = { ignore (ret (0)), ret(1) } in
      if (ret(x = 1))
        then { print('test15 passed\n'), [count] { set count := 1 } , get count }
        else { print('test15 failed\n'), get count }
}

;

fun test16p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    cmdlet x = {
        local t := 1 in {
            while (ret(false)) {
                ret(0)
            },
            get t
        }
     } in
      if (ret(x = 1))
        then { print('test16 passed\n'), [count] { set count := 1 } , get count }
        else { print('test16 failed\n'), get count }
}

;

fun test17p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    cmdlet x = {
        do (sum(10))
     } in
      if (ret(x = 55))
        then { print('test17 passed\n'), [count] { set count := 1 } , get count }
        else { print('test17 failed\n'), get count }
}

;

fun test18p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    cmdlet x = {
        if (ret(true)) then {
            ret(5050)
        } else {
            ret(0)
        }
     } in
      if (ret(x = 5050))
        then { print('test18 passed\n'), [count] { set count := 1 } , get count }
        else { print('test18 failed\n'), get count }
}

;

fun test19p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    cmdlet x = {
        if (ret(false)) then {
            ret(5050)
        } else {
            ret(0)
        }
     } in
      if (ret(x = 0))
        then { print('test19 passed\n'), [count] { set count := 1 } , get count }
        else { print('test19 failed\n'), get count }
}

;

fun test20p (u : unit) : cmd[nat] = cmd {
    local count := 0 in
    cmdlet x = {
        local t := 1 in {
            [t] { set t := 5050 },
            get t
        }
     } in
      if (ret(x = 5050))
        then { print('test20 passed\n'), [count] { set count := 1 } , get count }
        else { print('test20 failed\n'), get count }
}

;

/* gcd test */
bndcmdexp test1 = test1p () in
bndcmdexp test2 = test2p () in
bndcmdexp test3 = test3p () in
bndcmdexp test4 = test4p () in
bndcmdexp test5 = test5p () in
bndcmdexp test6 = test6p () in
bndcmdexp test7 = test7p () in
bndcmdexp test8 = test8p () in
bndcmdexp test9 = test9p () in
/* collatz test */
bndcmdexp test10 = test10p () in
bndcmdexp test11 = test11p () in
bndcmdexp test12 = test12p () in
bndcmdexp test13 = test13p () in
bndcmdexp test14 = test14p () in
/* sugar test */
bndcmdexp test15 = test15p () in
bndcmdexp test16 = test16p () in
bndcmdexp test17 = test17p () in
bndcmdexp test18 = test18p () in
bndcmdexp test19 = test19p () in
bndcmdexp test20 = test20p () in
    ret (test1 + test2 + test3
        + test4 + test5 + test6
        + test7 + test8 + test9
        + test10 + test11 + test12
        + test13 + test14 + test15
        + test16 + test17 + test18
        + test19 + test20
    )
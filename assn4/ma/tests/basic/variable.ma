/* result: 7 */

/*
 * Command that uses multiple local assignable as if they are expressions
 * Inside the parens, a and b are variables
 */
local a := 1 in
local b := 3 in {
    [a, b](a + b + b)
};
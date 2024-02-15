# Getting Started with Assignment 3

## Programming Tasks

Refer to the writeup for more detail and specifications.

- Implement `Index`, `Fun`, `Ap`, `IsInstance` cases of the dynamics of PyCF in `lang-pycf/dynamics-pycf.sml`.
- Implement `comprehension.pycf`, `sum_default.pycf`, and `matrix_sum.pycf` using concrete syntax as specified in the next section.
- Implement `new`, `cast`, and `fun` in `translate/synext.sml`.
    - Use the helper functions in `translate/synext-helpers.sml`.
    - Refer to `translate/synext.sig` for descriptions of helper functions.
- Implement the translation from PyCF to FPC in `translate/translate.fun`.
    - Use the functions you defined in `translate/synext.sml`.
    - You will not need to define your own labels.

# PyCF Concrete Syntax Tips

The concrete syntax closely resembles Python syntax, but has many fewer constructs. There are examples in `lang-pycf/tests` to which you can refer. Here are some general tips:
- There are only two declarations you can use: `def fun_name(input):` and `x = e`. These are functions and let bindings.
- Only `<=` is available to you as a comparator.
- You only have if expressions not if statements. Use `x if p else y` and you can nest them if needed.

## Testing

As always run the following from within the relevant folder

```sh
$ smlnj -m ./sources.cm
```

When testing your PyCF code, make sure to uncomment the `print` which you want to test, and comment out the last `print(name_of_fun)`. When you're done, restore the comments to their original state for the autograder.
To run the PyCF interpreter, first run SML in `lang-pycf` and then

```shell
- InterpreterPyCF.evalFile "tasks/name_of_task.pycf";
```

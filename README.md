# VSCode

Load Common Lisp file into VSCode REPL: `ALT+Shift+L`

# Equality operators

* `eq` is for by-reference equality. Useful if you want to check if two objects are the same object.
* `equal` is for by-value equality. This should be the one you use the most.
* everything else is garbage

# Variables

`defparameter` sets a dynamically scoped variable

```
(defparameter my-variable "variableValue")
```

`defvar` is like defparameter, but it works only if the variable has not been set before

```
(defvar my-variable "variableValue")
```


`let` sets a lexically scoped (normal) variable

```
(let ((my-variable "myValue"))
  (print my-variable))
```

### LET*

There is one problem with `let`. By specification, when we try to define several variables in a `let` expression, Lisp will try to define them in parallel.

This will cause problems if we reference a variable in the definition of another variable:

```
(let ((my-var 1) (my-var-2 (+ my-var 1)))
  (print my-var-2))
```

This will throw an error because we're referencing my-var into the assignment of my-var-2.

The solution is to use `let*`:

```
(let* ((my-var 1) (my-var-2 (+ my-var 1)))
  (print my-var-2))
```

# If, When

The normal conditional is `if`

If you dont have to handle the "else" case, it's better to use `when`. `when` acts as a `progn`, so it will handle any number of expressions. `if`, on the other hand, will interpret the first expression as the "true" case and the second as the "else case" (unless we explicitly use `progn`, that is).

```
(when my-pred
      (defparameter my-param "something") my-param)
```

# Cond, case

`cond` is the "traditional" conditional construct of Lisp. there is no reason to use it and it can be considered legacy.

`case` is Common Lisp's way to define what other languages would call a "match expression".

```
(case (+ 1 2)
      (5 "variant 1, five")
      ((2 3) "variant 2, two or three")
      (otherwise "variant 3, none of above")) ;returns "variant 2, two or three"
```

Notice that `case` uses `eql`, not `equal`. It can't be used for strings or lists.
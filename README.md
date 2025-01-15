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

Alexandria's `switch` on the other hand can be configured to use `equal` 

```
(alexandria:switch ("XY" :test 'equal)
  ("XY" "an X and a Y")
  ("AB" "an A and a B"))
```

# Looping - Definite loops

There are two fundamental things to remember in `loop`s

* `do` is for side effects

* `collect` is for returning

```
(loop for x in '(1 3 5)
            collect (+ x 1) ) ;returns '(2 4 6)


(loop for x in '(1 3 5)
            do (print x) )
```

To iterate over vectors, we use `across` instead of `in`

```
(loop for x across #(1 3 5)
            do (print x))
```

If we want to be able to iterate over both lists and vectors, we can use `being the element of`:

```
(loop for x being the element of #(1 3 5)
            do (print x))
```

### Iterating over ranges

`loop` supports iterating over ranges:

```
(defun print-range () (loop for x from 1 to 10 do (print x)))
```

# Looping - Indefinite loops

`loop` supports while loops too

```
(defun print-something-10-times () (let ((i 0))
                                     (loop while (< i 10)
                                           do (print "something")
                                             (setf i (+ i 1)))))
```

# Looping - Infinite loop

Common Lisp's way of writing a `while(true)` is the simplest form of the loop construct:

```
(loop (print "printing this forever"))
```

# Functions - default values

We can define default values for function parameters like this:

```
(defun print-name (&key (name "John") (surname "Doe"))
  (format t "Your name is ~a and your surname is ~a" name surname))

(print-name) ;prints "Your name is John and your surname is Doe"
```

Notice that if we were to provide values, we'd have to name the parameters

```
(print-name :name "Foo" :surname "Bar")
```

# Functions - variadic arguments

To provide variadic arguments (essentially what `params` does in C#) we use `&rest`

```
(defun get-max (&rest numbers) (loop for x in numbers maximizing x))

(get-max 1 9 10 99) ;returns 99
```

# Packages

You can install a package by running this inside of `sbcl`:

```
(ql:quickload "alexandria")
```

You can then call the functions by prefixing them with the package name:

```
(alexandria:switch ("XY" :test 'equal)
  ("XY" "an X and a Y")
  ("AB" "an A and a B"))
```

If you're unsure about the full name of a function, you can search for it with `apropos`:

```
(apropos "switch")
```
# VSCode

Load Common Lisp file into VSCode REPL: `ALT+Shift+L`

### Expand macro

1. Position yourself at the last parens of a macro call
2. Open VSCode's command palette
3. Select "Inspect macro"

![alt text](expanded.png)

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

# Functions - function references (sharp quote and lambda)

To reference functions in Common Lisp, we use a sharp quote (`#'`)

```
(defun say-hello ()
    (print "hello"))

(print #'say-hello)
(funcall #'say-hello)
```

Sharp quotes are used a lot to define higher order functions in Common Lisp:

```
(defun operation (a b &key(operation-type #'+))
    (funcall operation-type a b)
  )

(operation 5 5) ;returns 10
(operation 5 5 :operation-type #'-) ;returns 0
```

Alternatively, we could also use `lambda` functions for the same purpose:

```
(operation 5 5 :operation-type (lambda (a b)  (+ a b)))
```

# Functions - multiple return values

In Common Lisp we can return multiple values with `values`. This is similar in practice to returning a tuple in other languages.

To "destructure" the "tuple", we use `multiple-value-bind`.

```
(defun return-some-numbers ()
  (values 5 15 25))


(defun print-some-numbers ()
  (multiple-value-bind (first second third) (return-some-numbers)
    (print first)
    (print second)
    (print third)))
```

# Classes

We can define classes with `defclass`. We can instantiate them with `make-instance`.

```
(defclass person ()
    ())

(make-instance 'person)
```

We can define `slots` (fields) in the second form:

```
(defclass person ()
    (name age))
```

### Set fields

We can set the value of fields with `(setf (slot-value ...`:

```
(defparameter person-obj (make-instance 'person))
(setf (slot-value person-obj 'name) "John Doe")
```

### Get fields

We have several ways to get the value of a field.

We can get the value of fields with `slot-value`

```
(slot-value person-obj 'name)
```

We can also define a `reader` (that is, a getter) on the class definition

```
(defclass person ()
    ((name :reader name) (age :reader age)))
```

Then, we can access the value simply with `(name obj)`:

```
(defparameter person-obj (make-instance 'person))
(setf (slot-value person-obj 'name) "Johnny")
(name person-obj)
```

We could also define an `accessor` (which is similar to a reader, but it's also `setf`able, meaning that it's both a getter and a setter):

```
(defclass person ()
    ((name :accessor name) (age :accessor age)))

(defparameter person-obj (make-instance 'person))
(setf (name person-obj) "Johnny") ;sets the accessor
(name person-obj) ;gets the accessor
```

### Set default values for fields

We can set a default value for fields with the `:initform` keyword:

```
(defclass person ()
    ((name :initform "John Doe") (age :initform 20)))
```

### Set fields at instantiation time

We can set fields at instantiation time by using the `:initarg` symbol. This is in practice similar to constructors in other programming languages.

```
(defclass person ()
    ((name :initarg :name) (age :initarg :age)))

(defparameter person-obj (make-instance 'person :name "Johnny" :age 22))
```

### Print objects

We can print an object's contents like this:

```
(describe person-obj)
```

### Instance allocation vs class allocation

By default, fields are defined as being "instance allocated". This means that if we set a field to some value, that change will be reflected only for that one instance.

If we define a field as being "class allocated" instead, setting the field to some value will set it also for every other instance of that class.

In a way, this behavior is similar to "static fields" in other languages.

```
(defclass person ()
    ((name :initarg :name) (age :allocation :class))) ;age is class allocated


(defparameter johnny (make-instance 'person :name "Johnny"))
(defparameter bobby (make-instance 'person :name "Bobby"))

(setf (slot-value johnny 'age) 20) ;set Johnny's age to 20
(print (slot-value johnny 'age))
(print (slot-value bobby 'age)) ;Bobby's age is now also 20
```

# Symbols vs keywords

A symbol looks like `'foo`.  A keyword looks like `:foo`.

It's common to see both used as arguments to functions or macros.

Keywords are essentially **syntactic sugar**. A keyword is a symbol that gets interned in the **keyword package**l.

In other words, `:foo` is a shorthand for `keyword:foo`.

A normal symbol, on the other hand, gets interned in the package it's defined in.

In practice, this means that passing a keyword to some function that resides in a package different than your own is safer, since the keyword is interned in its own package and not yours or the function's. <br/>
For example, if you passed a symbol to a function that resides in a different package and the function checked for equality, the check would fail, since it would be checking if `'this-package:foo` equals to `'some-other-package:foo`. <br/>
In the same scenario, the check would pass with a keyword.

As a final note, keep in mind that `'symbol` is, too, a form of sytactic sugar. It stands for `(quote symbol)`

# Methods

Methods, unlike functions, can have typed parameters and overloading.

```
(defmethod say-hello ((my-dictionary hash-table))
  (print "Hello pretty dictionary")
  )


(defmethod say-hello ((my-string string))
  (print "Hello pretty string"))


(say-hello (make-hash-table)) ;prints "Hello pretty dictionary"
(say-hello "ciao") ;prints "Hello pretty string"
(say-hello 11) ;error: "There is no applicable method"
```

This behavior is called `generic functions` in Common Lisp parlance, and it's Common Lisp's own way to achieve **dynamic dispatch**.

An alternative to `defmethod` is `defgeneric`. The difference is basically that, with `defgeneric`, we essentially centralize all of the method's "specializations" in one single block.

```
(defgeneric say-hello (obj)
  (:method ((my-string string))
           (print "Hello pretty string"))
  (:method ((my-hash-table hash-table))
           (print "Hello pretty dictionary")))

(say-hello (make-hash-table)) ;prints "Hello pretty dictionary"
(say-hello "ciao") ;prints "Hello pretty string"
```

For most practical purposes, `defmethod` is recommended over `defgeneric`.

# Errors

In Lisp parlance, errors are **signaled**, not thrown.

To signal an error, we use the `error` function:

```
(error "error was signaled!")
```

Errors follow a hierarchy, much like classes. The simplest error type - which is the one we've thrown above - is called `SIMPLE-ERROR`.

`SIMPLE-ERROR` inherits from a more generic type, called a `SIMPLE-CONDITION`. A `condition` is not necessarily an error, but it still represents something that you want to "signal".

An example of another common condition type that is not an error are warnings, which we can signal with the `warn` function.

The full list of built-in condition types is noted below.

```
arithmetic-error                  floating-point-overflow   simple-type-error   
cell-error                        floating-point-underflow  simple-warning      
condition                         package-error             storage-condition   
control-error                     parse-error               stream-error        
division-by-zero                  print-not-readable        style-warning       
end-of-file                       program-error             type-error          
error                             reader-error              unbound-slot        
file-error                        serious-condition         unbound-variable    
floating-point-inexact            simple-condition          undefined-function  
floating-point-invalid-operation  simple-error              warning             
```

### Handling (catching)

In Lisp parlance, we "handle" conditions, we don't "catch" them.

To handle conditions, we use `handler-case`:

```
(handler-case (function-that-might-signal-a-condition)
  (error (c)
    (print "we got an error"))
  (warning (c)
           (print "we got a warning"))
  )
```

If we wanted to handle **all** conditions, we could replace `error` or `warning` with their superclass `simple-condition`.

Similarly, we could replace `error` or `warning` with a more specialized condition type, like `parse-error` or `division-by-zero`.

### Define custom conditions

We can define our own custom conditions that inherit from `simple-condition`

```
(define-condition my-condition-name (simple-condition) ())
```

Or also own our custom errors that inherit from `simple-error`:

```
(define-condition my-condition-name (simple-error) ())
```

TODO maybe inherit from error???????



### Unwind-protect (finally)

`unwind-protect` is basically Common Lisp's equivalent of what in other languages is called a `finally` block.

```
(defun finally-function ()
  (print "Cleaning up...."))

(defun function-that-might-signal-error ()
  (error "Something bad happened!"))

(unwind-protect (function-that-might-signal-error) (finally-function))
```

# Macros

Macros, unlike functions, do **NOT** evaluate their arguments.

```
(defun test-function (expr)
  (format nil "our expression is: ~s" expr))

(defmacro test-macro (expr)
  (format nil "our expression is: ~s" expr))

(test-function (+ 1 2)) ;returns 3
(test-macro (+ 1 2)) ;returns (+ 1 2)
```

Macros, unlike functions, run at **compile time**.

### Quotes

Quotes (`quote` or `'`) prevent evaluation of their arguments.

They can be used inside of macros, but also in regular code.

### Backquotes and commas

Commas (`,`) and backquotes (`` ` ``) are two features that are supposed to be used together. You must be inside a **backquoted** expression to use a comma.

Backquote is essentially the same as quote, it prevents the evaluation of its argument. The only difference is that it allows the use of `comma`s inside of it.

Commas are basically "exceptions" to the backquote. If you decorate an item with a comma, you're basically saying "I want to evaluate this thing, unlike the rest of the expression".

```
(let ((a 1)
      (b 2))
  `(a ,b)) ;returns (A 2)
```

Just like regular quotes, backquotes and commas can also be used in regular code, outside of macros.

### Functions vs Macros

* Macros do **not** evaluate their arguments
* Macros **cannot** be used as higher order functions
* Macros **cannot** be seen in a stacktrace or when you disassemble the code

### Comma splice

`Comma splice` (`,@`) can be thought of as a "flatmap" for Lisp lists. It can be used when you find that you have an extra "level" of parentheses.

### &body vs &rest

In macros, we'll often see `&body` being used in place of `&rest`.

The only difference is that `&body` instructs emacs to indent the arguments provided for a `&body` parameter at the same level. Identation aside, they are functionally completely identical.

### Gensym

`gensym` generates a symbol name that is guaranteed to be unique.

It's meant to be used inside of macros for variable names. This way you avoid conflicts with your runtime variables' names.

### CL flow

1. The Lisp reader reads your code, to check if it is syntactically sound (**read time**)
2. Macros expand to code (**macro expansion time**)
3. The code is compiled (**compile time**)
4. The code is ran (**run time**)

### Macros - a practical example

```
(defmacro with-long-compilation (&body body)
  (sleep 2)
  (format t "finished sleeping")
  `(progn ,@body))

(defun main ()
    (with-long-compilation (print "hi")))

(main)
```

The `with-long-compilation` is going to expand **to the last expression** (the return value). 

The first part of the macro (the 2 seconds of sleep and the format) are going to be ran by the compiler, but are not part of the expansion.

What is going to happen is then that:

1. Compilation is going to take 2+ seconds, due to the first two instructions
2. Once compilation is done, the macro is expanded to `(progn (print "hi"))`

### Macros - use cases

As we've seen in the example above, there are two main use cases for macros:

1. have the compiler run code, often to run validation logic
2. expand to code

In the example above, we're actually doing both things.

A famous lisp macro that uses exclusively the first pattern is `defun*`. [`defun*`](https://github.com/lisp-maintainers/defstar) lets you assign static types to function parameters and raises compiler errors if invalid arguments are provided.

### Macros - reader macros

A specific kind of macros, **reader macros**, expand at read time, rather than compile time.

Reader macros are ran with `#.` (sharp dot).

```
(defun create-function-name ()
    'my-second-function)

(defun #.(create-function-name) ()
    (print "hello from the function"))

(my-second-function) ;prints "hello from the function"
```

# Systems, packages

A **system** contains the metadata of your packages. When you `quickload` a library you're installing a Lisp **system**.

Systems are, conceptually, like Debian packages.

A **package**, on the other hand, is a container that contains symbols.

Packages are, conceptually, like namespaces in other languages.

When we start a new Lisp project, by default we're in the `common-lisp-user`. This package groups together all the functions and macros of the standard language. For example, `defun` or `let` are found inside this package.

# Work with existing projects

Projects on Github will have an `.asd` file. That's the system definition.

To load the system definition, we launch from an sbcl shell:

```
(asdf:load-asd "systemName.asd")
```

Then, we load it with Quicklisp (note that Quicklisp does double duty: it can load both projects from the internet or local ones):

```
(ql:quickload "systemName")
```

To switch from the `common-lisp-user` package to the Github project's package (thus being able to access internal functions) we run:

```
(in-package :packageName)
```

# Create your own project

The simplest `.asd` file you can have looks like this:

```
(in-package #:asdf-user)

(defsystem :my-project
  :depends-on (:alexandria :str :cl-ppcre :clingon)
  :components ((:file "my-project")
               (:static-file "README.md")))
```

Here we are saying that 
* our system is called `my-project` (`defsystem :my-project`)
* our dependencies are the libraries `alexandria`, `str`, `cl-ppcre`, `clingon`
* the source file is called `my-project.lisp` (`:file "my-project"`)
* we should include a static file in the project (`:static-file "README.md"`)

Then we'll have our `my-project.lisp` file:

```
(defpackage #:mypackage
  (:use :cl)
  (:export :my-entry-point))

(in-package :mypackage)

(defun my-entry-point ()
  (print "You're in the entry point"))
```

Here:

* we define a package called mypackage (`defpackage`)
* we import the `common-lisp-user` package (`:use :cl`)
* we get inside the `mypackage` package (`(in-package :mypackage)`)
* we define an entry point 

# Installing Packages

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
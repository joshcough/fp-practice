# FP Practice!!

This repo has a bunch of programming languages that need implementations for.

Each of the languages has a shell implementation with simple `???` that you
need to fill in.

The use of the term 'programming language' is used very loosely here, 
since most of the so called languages here are *not* programming languages.
They are not *Turing Complete*. Turing completeness is something we'll explore
later on in this project.

Here, I'll list (and describe) them in the order they should be implemented.

## ArithLang

ArithLang is the simplest language you will ever write.
It supports numbers, addition, and multiplication. That's it.

#### Grammar

The grammar is very simple:

    e   := nat | (+ e e) | (* e e)
    nat := natural numbers from 0 to (2^32)-1

#### Syntax Examples

Example expressions are:

* `7`
* `(+ 5 6)`
* `(* (+ 3 8) (* 2 7))`

#### Evaluation Examples

For the rest of this document, I will use ==> to mean 'evaluates to'. 
For example:

* `(+ 5 6) ==> 11` is the same as `(+ 5 6) evaluates to 11`.
* `(* (+ 3 8) (* 2 7)) ==> 99` is the same as `(* (+ 3 8) (* 2 7)) evaluates to 99`.
* `7 ==> 7` is the same as `7 evaluates to 7`.

## LetLang

This is ArithLang with let statements. All ArithLang expressions
are valid in LetLang, and it includes two additional expressions -
let statements and variable references.

#### Grammar

    e   := v | nat | (+ e e) | (* e e) | (let (v e) e)
    nat := natural numbers from 0 to (2^32)-1
    v   := a-z+

#### Syntax Examples

Let statements:

* `(let (x 6) (+ 5 x))`
* `(let (x 9) (* (+ 3 8) (* x 7)))`
* `(let (x 9) (let (y (+ x 6)) (* (+ y 3) (* x 7))))`

In Scala, the last expression would be: 

    { 
      val x = 9
      val y = x + 6
      (y + 3) * (x * 7)
    }

And simple variable references.

* `x`

#### Evaluation Examples

In the following example the binding expression of the let `(x 6)`,
assigns `x` a value of `6`. It is then available in the remainder
of the expression. In `(+ 5 x)`, `x` is *in scope*. Therefore:

* `(let (x 6) (+ 5 x)) ==> (+ 5 6)` and then `(+ 5 6) ==> 11`.

The semantics for expressions from ArithLang are the same.

The semantics for unbound variables are undefined (or result in error).
For example, the following program is invalid, because both x and y are unbound.

* `(+ x y) ==> Error: unbound variable.`

## LetAndPrintLang

LetAndPrintLang is LetLang with print statements, and statement blocks.

* Print statements take an expression, print the resulting value, and return it.
* Statement blocks run each expression in order, and return the result of the final expression.

#### Grammar

    e   := v | nat | (+ e e) | (* e e) | (let (v e) e) | (print e) | (statements e...)
    nat := natural numbers from 0 to (2^32)-1
    v   := a-z+
    
#### Syntax Examples

* `(print 9)`
* `(print (+ 5 6))`
* `((print 6) (print 7))`
* `((print 6) (print (+ 5 6)) (print (print 6)))`

Additionally, all LetLang expression are valid LetAndPrintLang expressions.

#### Evaluation Examples

LetAndPrintLang has slightly different semantics than LetLang and ArithLang.
Instead of simply returning a value, it should return a list  of all of the 
values that were printed in the program, *and* a value.

* `(print 9) ==> ((9), 9)`
* `((print 1) (print 2) (print 3)) ==> ((1,2,3), 3)`
* `((+ 1 (print 9)) (print 2) (print 3)) ==> ((9,2,3), 3)`
* `(+ (print 1) (print 9)) ==> ((1,9), 10)`

## MemoryLang

Coming soon.

#### Grammar
#### Syntax Examples
#### Evaluation Examples
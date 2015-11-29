# FP Practice!!

This repo is designed to help you learn functional programming
by studying and implementing programming languages.

Contained in the `writeme/src/main` directory are several programming
languages that need implementations. Each of the languages has a shell 
implementation with simple `???`s that you need to fill in.

Once you've written the interpreter for a language, you can test it
like this:

    sbt
    project writeme
    testOnly startHere.FirstTests

The use of the term 'programming language' is used very loosely here, 
since most of the so called languages here are *not* programming languages.
They are not *Turing Complete*. Turing completeness is something we'll explore
later on in this project.

Here's the recommended order in which you should implement the languages:

* `startHere.FirstLang`   - Simple arithmetic expressions.
* `let.LetLang`           - FirstLang, extended with let expressions.
* `print.PrintStdOut`     - FirstLang, extended with print statements/expressions.
* `print.PrintCollect`    - Like PrintStdOut, but with a twist.
* `print.LetAndPrint`     - Contains let *and* print expressions.
* `print.StatementBlocks` - Contains let, print, and statement blocks.
* `functions.Functions`   - Contains let and first class functions.
* `memory.Memory`         - Contains statement blocks, and memory, with get and set expressions.

However, each language is completely self-contained, so you could 
pick any of the languages to implement first. This is especially true
if you are experienced and want to move right to more advanced languages.

## Simple language summaries

* `startHere.FirstLang` - Simple arithmetic expressions.

In FirstLang you'll learn what an AST (abstract syntax tree) is,
and write your first interpreter. Otherwise, FirstLang is wholly
uninteresting - it exists simply to help get you started.

Concepts learned include __data types, recursion, interpreters,
abstract syntax trees__.

* `let.LetLang` - FirstLang, extended with let expressions.

LetLang is an extension of FirstLang that introduces you to
__let expressions__ (aka let bindings or variable bindings).
This helps you understand the concepts of __variable scope__ and 
__substitution__ in programming languages and why they are important 
(and can be tricky to get right).

Our initial LetLang interpreter implementation will have a fatal flaw -
 it will throw errors upon finding an unbound variable name. For 
example: `let x = 6 in x + y` is an invalid program because `y` is
__unbound__. The interpreter will throw an error `"unbound variable: y"`.
We will fix this later in the Error Handling section where we will
learn about several important FP data types, and eventually Monads.

A correct implementation of LetLang requires plumbing an environment
around, and it can be a little annoying. Later we will refactor LetLang
to remove this annoying plumbing, and that will introduce us to
the __Reader Monad__. We'll see how the reader monad can be used as
a wholesale replacement for Dependency Injection.

Concepts learned: __scope, type aliases, substitution__.

* `print.PrintStdOut` - FirstLang, extended with print statements/expressions.

PrintStdOut an extension of FirstLang (not LetLang) has print 
statements that, when evaluated, print their value to std out.
This introduces us to the concept of IO (really, just the O half of IO) 
and how and why that can be difficult to test. It also helps us start a
discussion about __referential transparency__ and what that means.
PrintStdOut print statements are not _referential transparent__.

Concepts learned: __referential transparency, side effects, IO__.

* `print.PrintCollect` - Like PrintStdOut, but with a twist.

PrintCollect is a revision (or refactoring) of PrintStdOut that
instead of printing to std out when evaluating a print statement,
collects the output of all the print statements in a list, and
returns them all (in the correct order) at the end of execution.
This makes the language much easier to test, and furthers our 
discussion of __referential transparency__. PrintCollect print 
statements are not __referential transparent__.

Like LetLang, a correct implementation of PrintCollect requires
plumbing an output list around, and it can be a very annoying. 
Later we will refactor PrintCollect to remove this annoying plumbing,
and that will introduce us to the __Writer Monad__. The writer monad 
can be used as a testable replacement for logging, without the plumbing.

Concepts learned: __referential transparency, side effects, IO__.

* `print.LetAndPrint` - Contains let *and* print expressions.

LetAndPrint brings together the LetLang and PrintCollect languages.
This doesn't actually introduce us to any new concepts, but it
is worth the effort of combining these two together. You will feel
the pain of having to plumb two different types of environments around.

Later on, this will lead us into a discussion about __Monad Transformers__,
and how they can be used to dramatically reduce unnecessary code in
your programs, while requiring minimal changes.

Concepts learned: no new concepts, reinforcement of previously learned 
concepts, and a possibly a discussion about some concepts we'll teach later.

* `print.StatementBlocks` - Contains let, print, and statement blocks.

StatementBlocks introduces (quite obviously) statement blocks, which
get added onto LetAndPrint. We'll have an interesting discussion 
about what statement blocks mean, and the difference between a 
statement and an expression. We'll also be introduced us to the 
concept of __fold__.

* `functions.Functions` - Contains let and first class functions

Functions is LetLang with __functions__ and __higher order functions__ added.
This will help us understand what a function really _is_. We'll discover
that let bindings are really just functions and function application
in disguise. 

Concepts learned: functions, higher order functions.

* `memory.Memory`

## In Depth Language Docs

### FirstLang

FirstLang is the simplest language you will ever write.
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

### LetLang

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

### LetAndPrint

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

### MemoryLang

Coming soon.

#### Grammar
#### Syntax Examples
#### Evaluation Examples
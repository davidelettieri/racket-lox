racket-lox
==========

Here my **failed** attempt at implementing Lox language from crafting interpreters book as a racket language module. 

# racket numbers note

I'm not able to control printing for racket numbers so tests for numbers value are changed with respect to crafting interpreters implementation.

I think the right racket representation for numbers is `flonums`, I'm forcing the number to be a `flonum` by adding a `#i` at the beginning of the string value before doing the conversion to number.

> Inexact real numbers are implemented as double-precision IEEE floating-point numbers, also known as flonums

[source](https://docs.racket-lang.org/reference/numbers.html#%28tech._flonum%29)

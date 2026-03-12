racket-lox
==========

Here my **failed** attempt at implementing Lox language from crafting interpreters book as a racket language module.

# racket numbers note

Scanner literals preserve exactness based on source representation: integer literals become exact integers while decimals become inexact reals (`flonums`).

Printing follows Crafting Interpreters behavior: whole-valued numbers are rendered without a trailing `.0` (for example `1` instead of `1.0`), while fractional values keep their decimal part.

> Inexact real numbers are implemented as double-precision IEEE floating-point numbers, also known as flonums

[source](https://docs.racket-lang.org/reference/numbers.html#%28tech._flonum%29)

How to run tests: raco test -x -p racket-lox

How to run single test file: raco test lang/parser-tests.rkt

How to run integration tests in a container (pinned Dart + Racket): ./run-tests.sh

By default integration tests use Podman when available.

How to force Docker instead of Podman: CONTAINER_CLI=docker ./run-tests.sh

How to update the package in the system: raco setup racket-lox

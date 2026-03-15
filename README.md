racket-lox
==========

This repo contains a "compliant" implementation of the Lox language from the [Crafting Interpreters book](https://craftinginterpreters.com/). By compliant I mean the implementation is passing all the tests up to the `chap13_inheritance` suite which is the last one of the Java part of the book.

The language can be used in DrRacket using `#lang racket-lox` at the top of the file. In order to pass the Lox tests, on syntax error, parse error, etc we need to exit the application with some specific error messages and error codes. This makes it incompatible with the default syntax error highlighting supported by DrRacket. I plan to have a "dialect" which supports that but it is not there yet.

How to install the package: raco pkg install

How to run tests: raco test -x -p racket-lox

How to run single test file: raco test lang/parser-tests.rkt

How to run integration tests in a container (pinned Dart + Racket): ./run-tests.sh

By default integration tests use Podman when available.

How to force Docker instead of Podman: CONTAINER_CLI=docker ./run-tests.sh

How to update the package in the system: raco setup racket-lox

Sample code

```
#lang racket-lox

print "Hello world!";
```

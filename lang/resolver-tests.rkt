#lang racket

(require rackunit
         "resolver.rkt"
         "parser.rkt"
         "scanner.rkt")

(define (resolve-string source)
  (define tokens (scan-tokens (open-input-string source)))
  (define statements (parse tokens))
  (resolve-statements statements))

(define (check-resolver-error source expected-part)
  (define error-out (open-output-string))
  (parameterize ([current-error-port error-out]
                 [exit-handler (lambda (code) (void))])
    (with-handlers ([exn:fail:lox:resolver? (lambda (e) (void))])
      (resolve-string source)))
  (define actual-output (get-output-string error-out))
  (check-regexp-match (regexp-quote expected-part) actual-output))

(module+ test
  (test-case "block scope redeclaration"
    (check-resolver-error "{ var a = \"first\"; var a = \"second\"; }"
                          "Already a variable with this name in this scope."))

  (test-case "return at top level"
    (check-resolver-error "return \"at top level\";" "Can't return from top-level code."))

  (test-case "this outside class"
    (check-resolver-error "print this;" "Can't use 'this' outside of a class."))

  (test-case "return value from initializer"
    (check-resolver-error "class Foo { init() { return \"something\"; } }"
                          "Can't return a value from an initializer."))

  (test-case "read local in initializer"
    (check-resolver-error "{ var a = a; }"
                          "Error at 'a': Can't read local variable in its own initializer."))

  (test-case "class inheriting from itself"
    (check-resolver-error "class Foo < Foo {}" "A class can't inherit from itself."))

  (test-case "super outside class"
    (check-resolver-error "print super.foo;" "Can't use 'super' outside of a class."))

  (test-case "super without superclass"
    (check-resolver-error "class Foo { method() { super.method(); } }"
                          "Can't use 'super' in a class with no superclass.")))

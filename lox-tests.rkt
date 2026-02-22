#lang racket

(require rackunit
         "lox.rkt")

(module+ test
  (test-case "lox-call matches parser-style lox-variable callee"
    (define ns (make-base-namespace))
    (define module-stx
      (datum->syntax #f
                     '(module lox-call-parser-style-test racket
                        (require (file "lox.rkt"))
                        (define (f a b)
                          (+ a b))
                        (define result (lox-call (lox-variable f) 1 2))
                        (provide result))))
    (parameterize ([current-namespace ns])
      (check-not-exn (lambda () (eval module-stx)))
      (check-equal? (dynamic-require ''lox-call-parser-style-test 'result) 3)))

  (test-case "lox-call works for class method access then invocation"
    (define ns (make-base-namespace))
    (define module-stx
      (datum->syntax #f
                     '(module lox-class-method-call-test racket
                        (require (file "lox.rkt"))
                        (lox-class Foo #f ((lox-function returnSelf () ((lox-return 1)))))
                        (define result (lox-call (lox-get (lox-call (lox-variable Foo)) returnSelf)))
                        (provide result))))
    (parameterize ([current-namespace ns])
      (check-not-exn (lambda () (eval module-stx)))
      (check-equal? (dynamic-require ''lox-class-method-call-test 'result) 1))))

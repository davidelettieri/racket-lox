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
      (check-equal? (dynamic-require ''lox-call-parser-style-test 'result) 3))))

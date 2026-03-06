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
      (check-equal? (dynamic-require ''lox-class-method-call-test 'result) 1)))

  (test-case "lox-this resolves to current instance in methods"
    (define ns (make-base-namespace))
    (define module-stx
      (datum->syntax #f
                     '(module lox-this-method-test racket
                        (require (file "lox.rkt"))
                        (lox-class Box
                                   #f
                                   ((lox-function set () ((lox-set (lox-this) value 7)))
                                    (lox-function get () ((lox-return (lox-get (lox-this) value))))))
                        (define box (lox-call (lox-variable Box)))
                        (lox-call (lox-get box set))
                        (define result (lox-call (lox-get box get)))
                        (provide result))))
    (parameterize ([current-namespace ns])
      (check-not-exn (lambda () (eval module-stx)))
      (check-equal? (dynamic-require ''lox-this-method-test 'result) 7)))

  (test-case "lox-this is available inside init"
    (define ns (make-base-namespace))
    (define module-stx
      (datum->syntax
       #f
       '(module lox-this-init-test racket
          (require (file "lox.rkt"))
          (lox-class Box
                     #f
                     ((lox-function init (v) ((lox-set (lox-this) value (lox-variable v))))
                      (lox-function get () ((lox-return (lox-get (lox-this) value))))))
          (define box (lox-call (lox-variable Box) 42))
          (define result (lox-call (lox-get box get)))
          (provide result))))
    (parameterize ([current-namespace ns])
      (check-not-exn (lambda () (eval module-stx)))
      (check-equal? (dynamic-require ''lox-this-init-test 'result) 42))))

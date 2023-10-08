#lang racket

(require (for-syntax racket/base))

(struct state (environment) #:mutable)

(define _state (state (make-hash)))

(define-syntax-rule (lox-define-var name value)
  (hash-set! (state-environment _state) name value))

(define lox-nil 'nil)

(define-syntax (lox-var-value stx)
  (syntax-case stx ()
    [(_ name)
     #'(hash-ref (state-environment _state) name (lambda () (undefined-variable name (syntax-line #'name))))]))

(define stderr (open-output-file "/dev/stderr" #:exists 'append))

(define (undefined-variable name line)
  (begin 
    (displayln (format "Undefined variable '~a'." name) stderr)
    (displayln (format "[line ~a] in script" line) stderr)
    (exit 70)))

(define-syntax (lox-assignment stx)
  (syntax-case stx ()
    [(_ name val)
        #'(begin 
            (hash-ref (state-environment _state) name (lambda () (undefined-variable name (syntax-line #'name))))
            (hash-set! (state-environment _state) name val)
            val)]))

(define-syntax lox-program
  (syntax-rules ()
    [(lox-program a) a]
    [(lox-program a ...) (begin a ...)]))

(define-syntax lox-declarations
  (syntax-rules ()
    [(lox-declarations a) a]
    [(lox-declarations a ...) (begin a ...)]))

(define-syntax-rule (lox-block a ...)
  (begin 
    (define previous (state-environment _state))
    (set-state-environment! _state (hash-copy previous))
    a ...
    (set-state-environment! _state previous)))

(define (lox-print value)
  (cond
   [(boolean? value) (print-bool value)]
   [(eqv? value 'nil) (displayln "nil")]
   [else (displayln value)]))

(define (print-bool value)
  (displayln (if value "true" "false")))

(provide lox-define-var
         lox-program
         lox-assignment
         lox-var-value
         lox-print
         lox-declarations
         lox-block
         lox-nil)


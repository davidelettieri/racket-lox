#lang racket

(require (for-syntax racket/base))

(define global-environment (make-hash))

(define-syntax-rule (lox-define-var name value)
  (hash-set! global-environment name value))

(define-syntax (lox-var-value stx)
  (syntax-case stx ()
    [(_ name)
     #'(hash-ref global-environment name (lambda () (undefined-variable name (syntax-line #'name))))]))

(define stderr (open-output-file "/dev/stderr" #:exists 'append))

(define-syntax-rule (undefined-variable name line)
  (begin 
    (displayln (format "Undefined variable '~a'." name) stderr)
    (displayln (format "[line ~a] in script" line) stderr)
    (exit 70)))

(define-syntax (lox-assignment stx)
  (syntax-case stx ()
    [(_ name val)
        #'(begin 
            (hash-ref global-environment name (lambda () (undefined-variable name (syntax-line #'name))))
            (hash-set! global-environment name val)
            val)]))

(define-syntax lox-program
  (syntax-rules ()
    [(lox-program a) a]
    [(lox-program a ...) (begin a ...)]))

(define-syntax lox-declarations
  (syntax-rules ()
    [(lox-declarations a) a]
    [(lox-declarations a ...) (begin a ...)]))

(define-syntax-rule (lox-print value)
  (displayln value))

(provide lox-define-var
         lox-program
         lox-assignment
         lox-var-value
         lox-print
         lox-declarations)


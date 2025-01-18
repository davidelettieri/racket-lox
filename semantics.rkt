#lang racket

(require racket/syntax)
(require (for-syntax racket/base racket/syntax))

(struct lox-state (environment) #:mutable #:transparent)

(define _state (lox-state (make-hash)))

(define-syntax-rule (lox-define-var name value)
  (hash-set! (lox-state-environment _state) name value))

; (define-syntax (lox-define-var stx)
;   (syntax-case stx ()
;     [(_ name val)
;      (with-syntax ([_name (format-id #'name "~a" #'name)])
;        (syntax (define _name val)))]))

(define lox-nil 'nil)

(define-syntax (lox-var-value stx)
  (syntax-case stx ()
    [(_ name)
      #'(hash-ref (lox-state-environment _state) name (lambda () (lox-runtime-error (format "Undefined variable '~a'." name) (syntax-line #'name))))]))
    ;  (with-syntax ([_name (format-id #'name "~a" #'name)])
    ;    (syntax _name))]))

(define stderr (open-output-file "/dev/stderr" #:exists 'append))

(define-syntax-rule (lox-empty-program)
  (void))

(define (lox-runtime-error message line)
  (begin
    (displayln message stderr)
    (displayln (format "[line ~a] in script" line) stderr)
    (exit 70)))

(define-syntax (lox-assignment stx)
  (syntax-case stx ()
    [(_ name val)
      #'(begin
      (hash-ref (lox-state-environment _state) name (lambda () (lox-runtime-error (format "Undefined variable '~a'." name) (syntax-line #'name))))
      (hash-set! (lox-state-environment _state) name val)
      val)]))
    ;  (with-syntax ([_name (format-id #'name "~a" #'name)])
    ;    (syntax (let ((x val)) (set! _name x) x)))]))

(define (lox-eqv? a b)
  (cond
    [(and (real? a) (nan? a)) #f]
    [(and (real? b) (nan? b)) #f]
    [else (eqv? a b)]))

(define-syntax lox-if
  (syntax-rules ()
    [(lox-if a b c) (if a b c)]
    [(lox-if a b) (when a b)]))

(define-syntax-rule (lox-block a ...)
  (let ()
    (define previous (lox-state-environment _state))
    (set-lox-state-environment! _state (hash-copy previous))
    a ...
    (set-lox-state-environment! _state previous)))

(define (lox-print value)
  (cond
    [(boolean? value) (print-bool value)]
    [(eqv? value 'nil) (displayln "nil")]
    [else (displayln value)]))

(define (print-bool value)
  (displayln (if value "true" "false")))

(define-syntax (lox-add stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [(_ a b) (syntax (lox-add-impl a b line))])))

(define (lox-add-impl left right line)
  (cond
    [(and (number? left) (number? right)) (+ left right)]
    [(and (string? left) (string? right)) (string-append left right)]
    [else (lox-runtime-error "Operands must be two numbers or two strings." line)]))

(define-syntax (lox-negate stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [(_ a) (syntax (lox-negate-impl a line))])))

(define (lox-negate-impl a line)
  (if (number? a) (- a) (lox-runtime-error "Operand must be a number." line)))

(define-syntax-rule (lox-binary-number-op name op)
  (define-syntax (name stx)
    (with-syntax ([line (syntax-line stx)]
                  [impl-id (format-id #'name "~a-impl" #'name)])
      (syntax-case stx ()
        [(_ a b) (syntax (define (impl-id a b) (op a b)))
                 (syntax (if (and (number? a) (number? b)) (op a b) (lox-runtime-error "Operands must be numbers." line)))]))))

(define-syntax-rule (lox-declarations head ...)
  (begin head ...))

(lox-binary-number-op lox-divide /)
(lox-binary-number-op lox-multiply *)
(lox-binary-number-op lox-subtract -)
(lox-binary-number-op lox-less <)
(lox-binary-number-op lox-less-equal <=)
(lox-binary-number-op lox-greater >)
(lox-binary-number-op lox-greater-equal >=)

(define-syntax-rule (lox-string s) s)
(define-syntax-rule (lox-number n) n)

(provide lox-define-var
         lox-assignment
         lox-var-value
         lox-print
         lox-block
         lox-nil
         lox-add
         lox-add-impl
         lox-divide
         lox-multiply
         lox-subtract
         lox-less
         lox-less-equal
         lox-greater
         lox-greater-equal
         lox-negate
         lox-negate-impl
         lox-number
         lox-string
         lox-if
         lox-declarations
         lox-eqv?
         lox-empty-program)


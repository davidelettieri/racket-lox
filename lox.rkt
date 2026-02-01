#lang racket

(require racket/syntax syntax/parse racket/stxparam)
(require (for-syntax racket/base racket/syntax syntax/parse))

(define lox-nil 'nil)

(define-syntax (lox-unary stx)
  (syntax-parse stx 
    #:datum-literals (BANG MINUS)
    [(_ BANG v:expr) #'(not v)]
    [(_ MINUS v:expr) #'(- v)]))

(define-syntax (lox-binary stx)
  (syntax-parse stx
    [(_ left:expr #\+ right:expr)
      (syntax/loc stx (lox-add left right))]
    [(_ left:expr #\- right:expr)
      (syntax/loc stx (lox-subtract left right))]
    [(_ left:expr #\> right:expr)
      (syntax/loc stx (lox-greater left right))]
    [(_ left:expr ">=" right:expr)
      (syntax/loc stx (lox-greater-equal left right))]
    [(_ left:expr #\< right:expr)
      (syntax/loc stx (lox-less left right))]
    [(_ left:expr "<=" right:expr)
      (syntax/loc stx (lox-less-equal left right))]
    [(_ left:expr #\/ right:expr)
      (syntax/loc stx (lox-divide left right))]
    [(_ left:expr #\* right:expr)
      (syntax/loc stx (lox-multiply left right))]
    [(_ left:expr "!=" right:expr)
      (syntax/loc stx (not (lox-eqv? left right)))]
    [(_ left:expr "==" right:expr)
      (syntax/loc stx (lox-eqv? left right))]))

(define (lox-eqv? a b)
  (cond
    [(and (real? a) (nan? a)) #f]
    [(and (real? b) (nan? b)) #f]
    [else (eqv? a b)]))

(define-syntax-parameter return-param 
  (lambda (stx) (raise-syntax-error #f "return used outside of function" stx)))

(define-syntax (lox-return stx)
    (syntax-parse stx
        [(_ val)
            #'(return-param val)]))

(define-syntax (lox-function stx)
  (syntax-parse stx
    [(_ name:id (arg:id ...) (stmt ...))
     #'(define (name arg ...)
         (let/ec k
           (syntax-parameterize ([return-param (make-rename-transformer #'k)])
             (lox-block stmt ...))))]))

(define-syntax (lox-add stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [(_ a b) (syntax (lox-add-impl a b line))])))

(define-syntax (lox-while stx)
  (syntax-parse stx
    [(_ cond:expr body:expr ...)
      #'(let loop ()
        (when cond
          body ...
          (loop)))]))

(define (lox-add-impl left right line)
  (cond
    [(and (number? left) (number? right)) (+ left right)]
    [(and (string? left) (string? right)) (string-append left right)]
    [else (lox-runtime-error "Operands must be two numbers or two strings." line)]))

(define-syntax-rule (lox-binary-number-op name op)
  (define-syntax (name stx)
    (with-syntax ([line (syntax-line stx)]
                  [impl-id (format-id #'name "~a-impl" #'name)])
      (syntax-case stx ()
        [(_ a b) (syntax (define (impl-id a b) (op a b)))
                 (syntax (if (and (number? a) (number? b)) (op a b) (lox-runtime-error "Operands must be numbers." line)))]))))

(lox-binary-number-op lox-divide /)
(lox-binary-number-op lox-multiply *)
(lox-binary-number-op lox-subtract -)
(lox-binary-number-op lox-less <)
(lox-binary-number-op lox-less-equal <=)
(lox-binary-number-op lox-greater >)
(lox-binary-number-op lox-greater-equal >=)

(define-syntax (lox-function-call stx)
  (syntax-parse stx
    [(_ name:expr arg1:expr ...)
     (with-syntax ([function (format-id #'name "~a" #'name)])
       #'(function arg1 ...))]))

(define-syntax (lox-var-declaration stx)
  (syntax-parse stx
    [(_ name:id val:expr)
       (syntax (define name val))]))

(define-syntax (lox-assign stx)
  (syntax-parse stx
    [(_ name:id val:expr)
      #'(begin
      (let [(c val)]
        (set! name c)
        c))]))

(define (lox-print value)
  (cond
    [(boolean? value) (print-bool value)]
    [(eqv? value 'nil) (displayln "nil")]
    [else (displayln value)]))

(define (print-bool value)
  (displayln (if value "true" "false")))

(define-syntax (lox-if stx)
  (syntax-parse stx
    [(_ cond then) #'(when cond then)]
    [(_ cond then else) #'(if cond then else)]))

(define-syntax (lox-call stx)
  (syntax-parse stx
    [(_ callee #f) #'(callee)]
    [(_ callee arg0 ...) #'(callee arg0 ...)]))

(define-syntax (lox-class stx)
  (syntax-parse stx
    ;; 1. match the whole structure including the method list shape
    [(_ name:id #f ((mname:id (marg:id ...) mbody:expr ...) ...))
     
     ;; 2. Create the class name identifier
     (with-syntax ([class-name (format-id #'name "~a%" #'name)])
       
       ;; 3. Output the final syntax
       #'(define class-name
           (class object%
             (super-new)
             ;; 4. Use the captured pattern variables directly
             (define/public (mname marg ...)
               mbody ...) ...)))]
    [(_ name:id superclass ((mname:id (marg:id ...) mbody:expr ...) ...))
     
     ;; 2. Create the class name identifier
     (with-syntax ([class-name (format-id #'name "~a%" #'name)]
                   [superclass-name (format-id #'superclass "~a%" #'superclass)])
       ;; 3. Output the final syntax
       #'(define class-name
           (class superclass-name
             (super-new)
             ;; 4. Use the captured pattern variables directly
             (define/public (mname marg ...)
               mbody ...) ...)))]))

(define (lox-runtime-error message line)
  (begin
    (displayln message (current-error-port))
    (displayln (format "[line ~a] in script" line) (current-error-port))
    (exit 70)))

(define-syntax (lox-variable stx)
  (syntax-parse stx
    [(_ name:id)
      (syntax name)]))

(define-syntax (lox-block stx)
  (syntax-parse stx
    [(_) #'(void)]
    [(_ a ...) #'(let () a ...)]))

(define-syntax-rule (lox-literal v)
  v)

(define-syntax-rule (lox-declarations head ...)
  (begin head ...))

(provide lox-unary
         lox-binary
         lox-function
         lox-return
         lox-nil
         lox-var-declaration
         lox-assign
         lox-print
         lox-add
         lox-block
         lox-declarations
         lox-class
         lox-literal
         lox-variable
         lox-if
         lox-while
         lox-call)

; (define lox-nil 'nil)



; (define-syntax-rule (lox-empty-program)
;   (void))





; (define-syntax lox-if
;   (syntax-rules ()
;     [(lox-if a b c) (if a b c)]
;     [(lox-if a b) (when a b)]))




; (define-syntax (lox-negate stx)
;   (with-syntax ([line (syntax-line stx)])
;     (syntax-case stx ()
;       [(_ a) (syntax (lox-negate-impl a line))])))

; (define (lox-negate-impl a line)
;   (if (number? a) (- a) (lox-runtime-error "Operand must be a number." line)))



; (define-syntax-rule (lox-string s) s)
; (define-syntax-rule (lox-number n) n)
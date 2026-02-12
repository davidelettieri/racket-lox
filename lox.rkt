#lang racket

(require racket/syntax
         racket/stxparam)
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/set))

(begin-for-syntax
  (define (resolve-redefinitions stmts)
    (define defined-vars (mutable-set))
    (define (replace-stmt stmt)
      (syntax-parse stmt
        #:datum-literals (lox-var-declaration)
        [(lox-var-declaration name:id val:expr)
         (define sym (syntax-e #'name))
         (if (set-member? defined-vars sym)
             #'(lox-assign name val)
             (begin
               (set-add! defined-vars sym)
               stmt))]
        [other #'other]))
    (map replace-stmt stmts)))

(define lox-nil 'nil)

(define-syntax (lox-unary stx)
  (syntax-parse stx
    #:datum-literals (BANG MINUS)
    [(_ BANG v:expr) #'(not (lox-truthy? v))]
    [(_ MINUS v:expr)
     (syntax/loc stx
       (lox-negate v))]))

(define-syntax (lox-negate stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [(_ a) (syntax (lox-negate-impl a line))])))
(define (lox-negate-impl a line)
  (if (number? a)
      (- a)
      (lox-runtime-error "Operand must be a number." line)))

(define-syntax (lox-binary stx)
  (syntax-parse stx
    #:datum-literals
    (PLUS MINUS GREATER GREATER_EQUAL LESS LESS_EQUAL SLASH STAR BANG_EQUAL EQUAL_EQUAL AND OR)
    [(_ left:expr PLUS right:expr)
     (syntax/loc stx
       (lox-add left right))]
    [(_ left:expr MINUS right:expr)
     (syntax/loc stx
       (lox-subtract left right))]
    [(_ left:expr GREATER right:expr)
     (syntax/loc stx
       (lox-greater left right))]
    [(_ left:expr GREATER_EQUAL right:expr)
     (syntax/loc stx
       (lox-greater-equal left right))]
    [(_ left:expr LESS right:expr)
     (syntax/loc stx
       (lox-less left right))]
    [(_ left:expr LESS_EQUAL right:expr)
     (syntax/loc stx
       (lox-less-equal left right))]
    [(_ left:expr SLASH right:expr)
     (syntax/loc stx
       (lox-divide left right))]
    [(_ left:expr STAR right:expr)
     (syntax/loc stx
       (lox-multiply left right))]
    [(_ left:expr BANG_EQUAL right:expr)
     (syntax/loc stx
       (not (lox-eqv? left right)))]
    [(_ left:expr EQUAL_EQUAL right:expr)
     (syntax/loc stx
       (lox-eqv? left right))]
    [(_ left:expr AND right:expr)
     (syntax/loc stx
       (lox-and left right))]
    [(_ left:expr OR right:expr)
     (syntax/loc stx
       (lox-or left right))]))

(define (lox-truthy? v)
  (not (or (eq? v #f) (eq? v lox-nil))))

(define-syntax (lox-or stx)
  (syntax-parse stx
    [(_ left:expr right:expr) #'(let ([l-val left]) (if (lox-truthy? l-val) l-val right))]))

(define-syntax (lox-and stx)
  (syntax-parse stx
    [(_ left:expr right:expr) #'(let ([l-val left]) (if (lox-truthy? l-val) right l-val))]))

(define (lox-eqv? a b)
  (cond
    [(and (real? a) (nan? a)) #f]
    [(and (real? b) (nan? b)) #f]
    [else (eqv? a b)]))

(define-syntax-parameter return-param
  (lambda (stx) (raise-syntax-error #f "return used outside of function" stx)))

(define-syntax (lox-return stx)
  (syntax-parse stx
    [(_ val) #'(return-param val)]))

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
         (when (lox-truthy? cond)
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
        [(_ a b)
         (syntax (define (impl-id a b)
                   (op a b)))
         (syntax (if (and (number? a) (number? b))
                     (op a b)
                     (lox-runtime-error "Operands must be numbers." line)))]))))

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
    [(_ name:id val:expr) (syntax (define name val))]))

(define-syntax (lox-assign stx)
  (syntax-parse stx
    [(_ name:id val:expr)
     (if (identifier-binding #'name)
         #'(begin
             (let ([c val])
               (set! name c)
               c))
         (with-syntax ([line (or (syntax-line #'name) (syntax-line stx) 0)]
                       [str-id (symbol->string (syntax-e #'name))])
           #'(lox-runtime-error (format "Undefined variable '~a'." str-id) line)))]))

(define (lox-print value)
  (cond
    [(boolean? value) (print-bool value)]
    [(eqv? value 'nil) (displayln "nil")]
    [else (displayln value)]))

(define (print-bool value)
  (displayln (if value "true" "false")))

(define-syntax (lox-if stx)
  (syntax-parse stx
    [(_ cond then)
     #'(when (lox-truthy? cond)
         then)]
    [(_ cond then else) #'(if (lox-truthy? cond) then else)]))

(define-syntax (lox-call stx)
  (syntax-parse stx
    [(_ callee arg0 ...)
     (with-syntax ([line (syntax-line stx)]
                   [param-count (length (syntax->list #'(arg0 ...)))])
       #'(let ([f callee])
           (if (procedure? f)
               (let ([arity (procedure-arity f)])
                 (if (eq? param-count arity)
                     (f arg0 ...)
                     (lox-runtime-error (format "Expected ~a arguments but got ~a." arity param-count)
                                        line)))
               (lox-runtime-error "Can only call functions and classes." line))))]))

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
    [(_ name:id) (syntax name)]))

(define-syntax (lox-block stx)
  (syntax-parse stx
    [(_) #'lox-nil]
    [(_ stmt ...) (expand-block-stmts #'(stmt ...))]))

(begin-for-syntax
  (define (expand-block-stmts stmts)
    (syntax-parse stmts
      [() #'lox-nil]
      [(stmt . rest)
       (syntax-parse #'stmt
         #:datum-literals (lox-var-declaration)
         [(lox-var-declaration name val)
          (with-syntax ([body (expand-block-stmts #'rest)])
            #'(let ([name val]) body))]
         [other
          (with-syntax ([body (expand-block-stmts #'rest)])
            #'(begin
                other
                body))])])))

(define-syntax-rule (lox-grouping expr)
  expr)

(define-syntax-rule (lox-literal v)
  v)

(define-syntax-rule (lox-declarations head ...)
  (begin
    head ...))

(define-syntax (lox-top stx)
  (syntax-parse stx
    [(_ . id:id)
     (with-syntax ([line (or (syntax-line #'id) (syntax-line stx) 0)]
                   [str-id (symbol->string (syntax-e #'id))])
       #'(lox-runtime-error (format "Undefined variable '~a'." str-id) line))]))

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
         lox-call
         lox-grouping
         lox-top
         (for-syntax resolve-redefinitions))

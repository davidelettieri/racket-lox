#lang racket/base

(require syntax/parse)

(provide resolve-statements)

;; The resolver primarily performs static analysis to enforce Lox semantic rules
;; that are not automatically handled by Racket's scoping (like redeclaration checks).
(define (resolve-statements stmts)
  (define scopes '()) ;; List of hash tables (symbol -> boolean/state)
  (define current-function 'none) ;; 'none | 'function | 'initializer | 'method
  (define current-class 'none) ;; 'none | 'class | 'subclass

  (define (begin-scope)
    (set! scopes (cons (make-hash) scopes)))

  (define (end-scope)
    (set! scopes (cdr scopes)))

  (define (declare name-stx)
    (unless (null? scopes)
      (define scope (car scopes))
      (define name (syntax-e name-stx))
      (when (and (hash-has-key? scope name)
                 (not (null? (cdr scopes)))) ;; Only check for duplicates in local scopes (not global)
        (raise-syntax-error #f "Already a variable with this name in this scope." name-stx))
      (hash-set! scope name #f))) ;; #f = declared, not defined (ready)

  (define (define-var name-stx)
    (unless (null? scopes)
      (define scope (car scopes))
      (define name (syntax-e name-stx))
      (hash-set! scope name #t))) ;; #t = defined (ready)

  (define (resolve-local name-stx)
    (define name (syntax-e name-stx))
    (unless (null? scopes)
      (define scope (car scopes))
      (when (and (hash-has-key? scope name) (eq? (hash-ref scope name) #f))
        (raise-syntax-error #f "Can't read local variable in its own initializer." name-stx))))

  (define (resolve-function params body type)
    (define enclosing-function current-function)
    (set! current-function type)
    (begin-scope)
    (for ([param (in-list params)])
      (declare param)
      (define-var param))
    (resolve-block-body body)
    (end-scope)
    (set! current-function enclosing-function))

  (define (resolve-block-body body)
    ;; Body might be a single expression or a list of statements depending on parser
    ;; But lox-block usually contains a sequence.
    ;; If body is (lox-block ...), we unwrap it.
    (syntax-parse body
      #:datum-literals (lox-block)
      [(lox-block stmt ...)
       (for ([s (in-list (attribute stmt))])
         (resolve-stmt s))]
      [stmt (resolve-stmt #'stmt)]))

  (define (resolve-stmt stmt)
    (syntax-parse stmt
      #:datum-literals (lox-block lox-var-declaration
                                  lox-function
                                  lox-class
                                  lox-if
                                  lox-while
                                  lox-print
                                  lox-return
                                  lox-expression-statement)
      [(lox-block stmt ...)
       (begin-scope)
       (for ([s (in-list (attribute stmt))])
         (resolve-stmt s))
       (end-scope)]
      [(lox-var-declaration name init)
       (declare #'name)
       (resolve-expr #'init)
       (define-var #'name)]
      [(lox-function name (param ...) body)
       (declare #'name)
       (define-var #'name)
       (resolve-function (attribute param) #'body 'function)]
      [(lox-class name super methods)
       (define enclosing-class current-class)
       (set! current-class 'class)
       (declare #'name)
       (define-var #'name)
       (unless (equal? (syntax-e #'super) #f)
         (set! current-class 'subclass)
         (when (eq? (syntax-e #'name) (syntax-e #'super))
           (raise-syntax-error #f "A class can't inherit from itself." #'super))
         (begin-scope)
         (hash-set! (car scopes) 'super #t))

       (begin-scope)
       (hash-set! (car scopes) 'this #t)

       (syntax-parse #'methods
         [(method ...)
          (for ([m (in-list (attribute method))])
            (syntax-parse m
              [(lox-function mname (mparam ...) mbody)
               (define declaration 'method)
               (when (equal? (syntax-e #'mname) 'init)
                 (set! declaration 'initializer))
               (resolve-function (attribute mparam) #'mbody declaration)]))])

       (end-scope)
       (unless (equal? (syntax-e #'super) #f)
         (end-scope))
       (set! current-class enclosing-class)]
      [(lox-if cond then)
       (resolve-expr #'cond)
       (resolve-stmt #'then)]
      [(lox-if cond then else)
       (resolve-expr #'cond)
       (resolve-stmt #'then)
       (resolve-stmt #'else)]
      [(lox-print val) (resolve-expr #'val)]
      [(lox-return val)
       (when (eq? current-function 'none)
         (raise-syntax-error #f "Can't return from top-level code." stmt))
       (when (and (eq? current-function 'initializer) (not (equal? (syntax-e #'val) 'lox-nil)))
         (raise-syntax-error #f "Can't return a value from an initializer." stmt))
       (resolve-expr #'val)]
      [(lox-while cond body)
       (resolve-expr #'cond)
       (resolve-stmt #'body)]
      [(lox-expression-statement expr) (resolve-expr #'expr)]
      ;; If it doesn't match a statement, treat as expression (expression statement)
      [expr (resolve-expr #'expr)]))

  (define (resolve-expr expr)
    (syntax-parse expr
      #:datum-literals (lox-assign lox-variable
                                   lox-binary
                                   lox-unary
                                   lox-call
                                   lox-literal
                                   lox-grouping
                                   lox-this
                                   lox-super
                                   lox-or
                                   lox-and
                                   lox-nil)
      [(lox-variable name) (resolve-local #'name)]
      [(lox-assign name val)
       (resolve-expr #'val)
       (resolve-local #'name)]
      [(lox-binary left op right)
       (resolve-expr #'left)
       (resolve-expr #'right)]
      [(lox-unary op right) (resolve-expr #'right)]
      [(lox-call callee arg ...)
       (resolve-expr #'callee)
       (for ([a (in-list (attribute arg))])
         (resolve-expr a))]
      [(lox-or left right)
       (resolve-expr #'left)
       (resolve-expr #'right)]
      [(lox-and left right)
       (resolve-expr #'left)
       (resolve-expr #'right)]
      [(lox-grouping e) (resolve-expr #'e)]
      [(lox-literal v) (void)]
      [lox-nil (void)]
      [(lox-this keyword)
       (when (eq? current-class 'none)
         (raise-syntax-error #f "Can't use 'this' outside of a class." #'keyword))]
      [(lox-super keyword method)
       (when (eq? current-class 'none)
         (raise-syntax-error #f "Can't use 'super' outside of a class." #'keyword))
       (when (not (eq? current-class 'subclass))
         (raise-syntax-error #f "Can't use 'super' in a class with no superclass." #'keyword))]
      [_ (void)]))

  (for ([s stmts])
    (resolve-stmt s))

  stmts)

#lang racket/base

(require syntax/parse)

(provide resolve-statements
         (struct-out exn:fail:lox:resolver))

(struct exn:fail:lox:resolver exn:fail (line) #:transparent)

;; The resolver primarily performs static analysis to enforce Lox semantic rules
;; that are not automatically handled by Racket's scoping (like redeclaration checks).
(define (resolve-statements stmts)
  (define scopes '()) ;; List of hash tables (symbol -> boolean/state)
  (define current-function 'none) ;; 'none | 'function | 'initializer | 'method
  (define current-class 'none) ;; 'none | 'class | 'subclass
  (define had-error? #f)

  (define (stx->error-element stx)
    (define datum (syntax->datum stx))
    (cond
      [(symbol? datum)
       (define symbol-name (symbol->string datum))
       (if (and (>= (string-length symbol-name) 4) (string=? (substring symbol-name 0 4) "lox-"))
           (substring symbol-name 4)
           symbol-name)]
      [(string? datum) datum]
      [(number? datum) (number->string datum)]
      [(boolean? datum) (if datum "true" "false")]
      [(and (pair? datum) (symbol? (car datum))) (stx->error-element (datum->syntax #f (car datum)))]
      [else "expression"]))

  (define (resolve-error stx message)
    (displayln
     (format "[line ~a] Error at '~a': ~a" (syntax-line stx) (stx->error-element stx) message)
     (current-error-port))
    (set! had-error? #t))

  (define (begin-scope)
    (set! scopes (cons (make-hash) scopes)))

  (define (end-scope)
    (set! scopes (cdr scopes)))

  (define (declare name-stx)
    (unless (null? scopes)
      (define scope (car scopes))
      (define name (syntax-e name-stx))
      (when (hash-has-key? scope name)
        (resolve-error name-stx "Already a variable with this name in this scope."))
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
        (resolve-error name-stx "Can't read local variable in its own initializer."))))

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
    ;; Body is a list of statements for lox-function in parser.
    ;; Or maybe lox-block wrapper.
    (syntax-parse body
      #:datum-literals (lox-block)
      [(lox-block stmt ...)
       (for ([s (in-list (attribute stmt))])
         (resolve-stmt s))]
      [(stmt ...)
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
           (resolve-error #'super "A class can't inherit from itself."))
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
         (resolve-error stmt "Can't return from top-level code."))
       (when (and (eq? current-function 'initializer) (not (equal? (syntax-e #'val) 'lox-nil)))
         (resolve-error stmt "Can't return a value from an initializer."))
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
         (resolve-error #'keyword "Can't use 'this' outside of a class."))]
      [(lox-super keyword method)
       (when (eq? current-class 'none)
         (resolve-error #'keyword "Can't use 'super' outside of a class."))
       (when (not (eq? current-class 'subclass))
         (resolve-error #'keyword "Can't use 'super' in a class with no superclass."))
       (resolve-local #'keyword)]
      [(lox-literal v)
       (void)] ; Re-add catch-all or literals handled above? Literal v is handled above.
      ;; We need a catch-all if expr can be something else.
      ;; But looking at the list of datum-literals, it seems exhaustive for Lox AST if correct.
      ;; However, if we missed something, it's safer to have [_ (void)].
      [_ (void)]))

  (for ([s stmts])
    (resolve-stmt s))

  (when had-error?
    (exit 65))

  stmts)

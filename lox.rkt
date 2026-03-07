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

(define-syntax-parameter this-param
  (lambda (stx) (raise-syntax-error #f "this used outside of class" stx)))

(define-syntax-parameter super-param
  (lambda (stx) (raise-syntax-error #f "super used outside of class" stx)))

(define current-call-line (make-parameter 0))
(define lox-get-method-msg '__lox-get-method__)

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
         (syntax (let ([av a]
                       [bv b])
                   (if (and (number? av) (number? bv))
                       (op av bv)
                       (lox-runtime-error "Operands must be numbers." line))))]))))

(lox-binary-number-op lox-divide /)
(lox-binary-number-op lox-multiply *)
(lox-binary-number-op lox-subtract -)
(lox-binary-number-op lox-less <)
(lox-binary-number-op lox-less-equal <=)
(lox-binary-number-op lox-greater >)
(lox-binary-number-op lox-greater-equal >=)

(define-syntax (lox-var-declaration stx)
  (syntax-parse stx
    [(_ name:id val:expr) (syntax (define name val))]))

(define-syntax (lox-assign stx)
  (syntax-parse stx
    [(_ name:id val:expr)
     (if (identifier-binding #'name)
         #'(let ([c val])
             (set! name c)
             c)
         (with-syntax ([line (or (syntax-line #'name) (syntax-line stx) 0)]
                       [str-id (symbol->string (syntax-e #'name))])
           #'(lox-runtime-error (format "Undefined variable '~a'." str-id) line)))]))

(define (lox-print value)
  (cond
    [(boolean? value) (print-bool value)]
    [(eqv? value 'nil) (displayln "nil")]
    [(lox-class-constructor? value) (displayln (lox-class-constructor-name value))]
    [(lox-class-instance? value) (displayln (format "~a instance" (lox-class-instance-name value)))]
    [(procedure? value)
     (let ([function-name (object-name value)])
       (if (eqv? function-name 'clock)
           (displayln "<native fn>")
           (displayln (format "<fn ~a>" function-name))))]
    [else (displayln value)]))

(define (print-bool value)
  (displayln (if value "true" "false")))

(define (lox-call-impl f args line)
  (define param-count (length args))
  (if (and (procedure? f) (not (lox-class-instance? f)))
      (if (or (lox-class-constructor? f) (procedure-arity-includes? f param-count))
          (parameterize ([current-call-line line])
            (apply f args))
          (lox-runtime-error
           (format "Expected ~a arguments but got ~a." (procedure-arity f) param-count)
           line))
      (lox-runtime-error "Can only call functions and classes." line)))

(define-syntax (lox-if stx)
  (syntax-parse stx
    [(_ cond then)
     #'(when (lox-truthy? cond)
         then)]
    [(_ cond then else) #'(if (lox-truthy? cond) then else)]))

(define-syntax (lox-call stx)
  (syntax-parse stx
    [(_ callee arg0 ...)
     (with-syntax ([line (syntax-line stx)])
       #'(lox-call-impl callee (list arg0 ...) line))]))

(struct lox-class-constructor (base name lookup superclass)
  #:property prop:procedure
  (struct-field-index base))
(struct lox-class-instance (base name fields) #:property prop:procedure (struct-field-index base))

(define (lox-class-find-method klass prop receiver)
  (cond
    [(not klass) #f]
    [else
     (define maybe-local ((lox-class-constructor-lookup klass) prop receiver))
     (if maybe-local
         maybe-local
         (lox-class-find-method (lox-class-constructor-superclass klass) prop receiver))]))

(define (lox-super-impl superclass receiver method-sym line)
  (if (lox-class-constructor? superclass)
      (let ([method (lox-class-find-method superclass method-sym receiver)])
        (if method
            method
            (lox-runtime-error (format "Undefined property '~a'." method-sym) line)))
      (lox-runtime-error "Superclass must be a class." line)))

(define (lox-get-impl o method-sym line)
  (cond
    [(lox-class-instance? o)
     (define fields (lox-class-instance-fields o))
     (cond
       [(hash-has-key? fields method-sym) (hash-ref fields method-sym)]
       [else
        (define maybe-method (o lox-get-method-msg method-sym))
        (if maybe-method
            maybe-method
            (lox-runtime-error (format "Undefined property '~a'." method-sym) line))])]
    [else (lox-runtime-error "Only instances have properties." line)]))

(define (lox-set-impl o method-sym value line)
  (cond
    [(lox-class-instance? o)
     (hash-set! (lox-class-instance-fields o) method-sym value)
     value]
    [else (lox-runtime-error "Only instances have fields." line)]))

(define-syntax (lox-class stx)
  (syntax-parse stx
    #:datum-literals (lox-function)
    [(_ class-name:id superclass ((lox-function m-name:id (m-arg:id ...) (m-body:expr ...)) ...))
     #:do [(define methods-list (syntax->list #'(((m-name m-arg ...) m-body ...) ...)))
           (define has-super? (syntax-e #'superclass))]
     (with-syntax ([(((method-name method-arg ...) method-body ...) ...) methods-list]
                   [class-line (or (syntax-line #'class-name) (syntax-line stx) 0)]
                   [superclass-expr (if has-super? #'superclass #'#f)])
       #'(define class-name
           (let ([superclass-value superclass-expr])
             (when (and superclass-value (not (lox-class-constructor? superclass-value)))
               (lox-runtime-error "Superclass must be a class." class-line))
             (define (lookup-local-method prop receiver)
               (case prop
                 [(method-name)
                  (procedure-rename
                   (lambda (method-arg ...)
                     (let ([this receiver]
                           [super superclass-value])
                       (define result
                         (let/ec k
                           (syntax-parameterize ([return-param (make-rename-transformer #'k)]
                                                 [this-param (make-rename-transformer #'this)]
                                                 [super-param (make-rename-transformer #'super)])
                             (lox-block method-body ...))))
                       (if (eq? 'method-name 'init) this result)))
                   'method-name)] ...
                 [else #f]))
             (lox-class-constructor
              (lambda ctor-args
                (define fields (make-hash))
                (define self #f)
                (set! self
                      (lox-class-instance (lambda (msg . args)
                                            (cond
                                              [(eq? msg lox-get-method-msg)
                                               (define prop (car args))
                                               (define field-present? (hash-has-key? fields prop))
                                               (if field-present?
                                                   (hash-ref fields prop)
                                                   (lox-class-find-method class-name prop self))]
                                              [else #f]))
                                          (symbol->string (syntax-e #'class-name))
                                          fields))
                (define maybe-init (lox-class-find-method class-name 'init self))
                (when maybe-init
                  (lox-call-impl maybe-init ctor-args (current-call-line)))
                (when (and (not maybe-init) (not (null? ctor-args)))
                  (lox-runtime-error (format "Expected 0 arguments but got ~a." (length ctor-args))
                                     (current-call-line)))
                self)
              (symbol->string (syntax-e #'class-name))
              lookup-local-method
              superclass-value))))]))

(define (lox-runtime-error message line)
  (begin
    (displayln message (current-error-port))
    (displayln (format "[line ~a] in script" line) (current-error-port))
    (exit 70)))

(define-syntax (lox-variable stx)
  (syntax-parse stx
    [(_ name:id) (syntax name)]))

(define-syntax (lox-this stx)
  (syntax-parse stx
    [_ #'this-param]))

(define-syntax (lox-super stx)
  (syntax-parse stx
    [(_ _ method:str)
     (with-syntax ([method-sym (string->symbol (syntax-e #'method))]
                   [line (or (syntax-line #'method) (syntax-line stx) 0)])
       #'(lox-super-impl super-param this-param 'method-sym line))]))

(define-syntax (lox-get stx)
  (syntax-parse stx
    [(_ obj method:str)
     (with-syntax ([method-sym (string->symbol (syntax-e #'method))]
                   [line (or (syntax-line #'method) (syntax-line stx) 0)])
       #'(lox-get-impl obj 'method-sym line))]))

(define-syntax (lox-set stx)
  (syntax-parse stx
    [(_ obj method:str value:expr)
     (with-syntax ([method-sym (string->symbol (syntax-e #'method))]
                   [line (or (syntax-line #'method) (syntax-line stx) 0)])
       #'(lox-set-impl obj 'method-sym value line))]))

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
         #:datum-literals (lox-var-declaration lox-function)
         [(lox-var-declaration name val)
          (with-syntax ([body (expand-block-stmts #'rest)])
            #'(let ([name val]) body))]
         [(lox-function name (arg ...) (fstmt ...))
          (with-syntax ([body (expand-block-stmts #'rest)])
            #'(letrec ([name (lambda (arg ...)
                               (let/ec k
                                 (syntax-parameterize ([return-param (make-rename-transformer #'k)])
                                   (lox-block fstmt ...))))])
                body))]
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
         lox-this
         lox-super
         lox-if
         lox-while
         lox-call
         lox-grouping
         lox-top
         lox-get
         lox-set
         (for-syntax resolve-redefinitions))

#lang racket

(require "helpers.rkt"
         "scanner.rkt")
(require syntax/parse
         racket/trace)

(struct exn:fail:lox exn:fail (line) #:transparent)

(define (token->src t)
  (token-srcloc t))

(define (token->symbol t)
  (datum->syntax #f (string->symbol (token-lexeme t)) (token->src t)))

(define (parse tokens)
  (define (synchronize)
    (advance)
    (let loop ()
      (unless (is-at-end?)
        (if (eqv? (token-type (previous)) 'SEMICOLON)
            (void)
            (case (token-type (peek))
              [(CLASS FUN VAR FOR IF WHILE PRINT RETURN) (void)]
              [else
               (advance)
               (loop)])))))
  (define _hadError #f)
  (define _tokens (list->vector tokens))
  (define _current 0)
  (define (advance)
    (when (not (is-at-end?))
      (set! _current (+ _current 1)))
    (previous))
  (define (consume type [message ""])
    (if (check type)
        (advance)
        (let* ([tok (peek)]
               [msg (if (string=? message "")
                        (format "Expect ~a." type)
                        message)])
          (parse-error tok msg))))
  (define (parse-error token message)
    (raise (exn:fail:lox (format "[line ~a] Error at '~a': ~a"
                                 (srcloc-line (token-srcloc token))
                                 (token-lexeme token)
                                 message)
                         (current-continuation-marks)
                         (srcloc-line (token-srcloc token)))))
  (define (check type)
    (and (not (is-at-end?)) (eqv? (token-type (peek)) type)))
  (define (check-next type)
    (and (not (is-at-end?))
         (not (eqv? (token-type (vector-ref _tokens (+ _current 1))) 'EOF))
         (eqv? (token-type (vector-ref _tokens (+ _current 1))) type)))
  (define (previous)
    (vector-ref _tokens (- _current 1)))
  (define (peek)
    (vector-ref _tokens _current))
  (define (is-at-end?)
    (eqv? (token-type (peek)) 'EOF))
  (define (match .
            token-types)
    (define matched (findf check token-types))
    (if matched
        (begin
          (advance)
          #t)
        #f))
  (define (declaration)
    (cond
      [(match 'CLASS) (class-declaration)]
      [(and (check 'FUN) (check-next 'IDENTIFIER))
       (begin
         (consume 'FUN)
         (function "function"))]
      [(match 'VAR) (var-declaration)]
      [else (statement)]))
  (define (class-declaration)
    (define name (consume 'IDENTIFIER "Expect class name."))
    (define superclass #f)
    (when (match 'LESS)
      (consume 'IDENTIFIER "Expect superclass name.")
      (define superclassToken (previous))
      (set! superclass
            (datum->syntax #f
                           (string->symbol (token-lexeme superclassToken))
                           (token->src superclassToken))))
    (consume 'LEFT_BRACE "Expect '{' before class body.")
    (define methods '())
    (while (not (and (check 'RIGHT_BRACE) (not (is-at-end?))))
           (set! methods (cons (function "method") methods)))
    (consume 'RIGHT_BRACE "Expect '}' after class body.")
    (define name-id (token->symbol name))
    (datum->syntax #f `(lox-class ,name-id ,superclass ,methods) (token->src name)))
  (define (function kind)
    (define name (consume 'IDENTIFIER (format "Expect ~a name." kind)))
    (consume 'LEFT_PAREN (format "Expect '(' after ~a name." kind))
    (define count 0)
    (define parameters
      (if (check 'RIGHT_PAREN)
          empty
          (for/list ([param (in-producer (lambda () (consume 'IDENTIFIER "Expect parameter name")))]
                     #:final (check 'RIGHT_PAREN))
            (set! count (+ count 1))
            (match 'COMMA)
            (when (> count 255)
              (parse-error (previous) "Can't have more than 255 parameters."))
            param)))
    (consume 'RIGHT_PAREN "Expect ')' after parameters.")
    (consume 'LEFT_BRACE (format "Expect '{' before ~a body" kind))
    (define body (block))
    (define name-id (token->symbol name))
    (datum->syntax #f
                   `(lox-function ,name-id ,(map token->symbol parameters) ,body)
                   (token->src name)))
  (define (var-declaration)
    (define name (consume 'IDENTIFIER "Expect variable name."))
    (define initializer
      (if (match 'EQUAL)
          (expression)
          #'lox-nil))
    (consume 'SEMICOLON "Expect ';' after variable declaration.")
    (define name-id (token->symbol name))
    (datum->syntax #f `(lox-var-declaration ,name-id ,initializer) (token->src name)))
  (define (block-statement)
    (define brace (previous))
    (define statements (block))
    (datum->syntax #f `(lox-block ,@statements) (token->src brace)))
  (define (block)
    (define statements
      (if (or (check 'RIGHT_BRACE) (is-at-end?))
          '()
          (for/list ([decl (in-producer protected-declaration)]
                     #:final (or (check 'RIGHT_BRACE) (is-at-end?))
                     #:when (lambda (el) (not (null? el))))
            decl)))
    (consume 'RIGHT_BRACE "Expect '}' after block.")
    statements)
  (define (for-statement)
    (define for-keyword (previous))
    (consume 'LEFT_PAREN "Expect '(' after 'for'.")
    (define initializer
      (cond
        [(match 'SEMICOLON) #f]
        [(match 'VAR) (var-declaration)]
        [else (expression-statement)]))
    (define condition
      (if (check 'SEMICOLON)
          (datum->syntax #f `(lox-literal #t) (token->src for-keyword))
          (expression)))
    (consume 'SEMICOLON "Expect ';' after loop condition.")
    (define increment
      (if (check 'RIGHT_PAREN)
          #f
          (expression)))
    (consume 'RIGHT_PAREN "Expect ')' after for clauses.")
    (define body (statement))
    (when increment
      (set! body (datum->syntax #f `(lox-block ,body ,increment) (token->src for-keyword))))
    (set! body (datum->syntax #f `(lox-while ,condition ,body) (token->src for-keyword)))
    (when initializer
      (set! body (datum->syntax #f `(lox-block ,initializer ,body) (token->src for-keyword))))
    body)
  (define (if-statement)
    (define if-keyword (previous))
    (consume 'LEFT_PAREN "Expect '(' after if.")
    (define expr (expression))
    (consume 'RIGHT_PAREN "Expect ')' after if.")
    (define then (statement))
    (if (match 'ELSE)
        (datum->syntax #f `(lox-if ,expr ,then ,(statement)) (token->src if-keyword))
        (datum->syntax #f `(lox-if ,expr ,then) (token->src if-keyword))))
  (define (return-statement)
    (define keyword (previous))
    (define value
      (if (check 'SEMICOLON)
          (datum->syntax #f 'lox-nil)
          (expression)))
    (consume 'SEMICOLON "Expect ';' after return value.")
    (datum->syntax #f `(lox-return ,value) (token->src keyword)))
  (define (while-statement)
    (define while-keyword (previous))
    (consume 'LEFT_PAREN "Expect '(' after 'while'.")
    (define expr (expression))
    (consume 'RIGHT_PAREN "Expect ')' after condition.")
    (define stmt (statement))
    (datum->syntax #f `(lox-while ,expr ,stmt) (token->src while-keyword)))
  (define (expression-statement)
    (define value (expression))
    (consume 'SEMICOLON "Expect ';' after expression.")
    value)
  (define (finish-call callee)
    (define paren (previous))
    (define arguments
      (if (check 'RIGHT_PAREN)
          empty
          (for/list ([arg (in-producer expression)]
                     #:final (check 'RIGHT_PAREN))
            (match 'COMMA)
            arg)))
    (consume 'RIGHT_PAREN "Expect ')' after arguments.")
    (datum->syntax #f `(lox-call ,callee ,@arguments) (token->src paren)))
  (define (call)
    (define expr (primary))
    (define c #t)
    (while c
           (cond
             [(match 'LEFT_PAREN) (set! expr (finish-call expr))]
             [(match 'DOT)
              (begin
                (define dot (previous))
                (define name (consume 'IDENTIFIER "Expect property name after '.'."))
                (set! expr (datum->syntax #f `(lox-get ,expr ,name) (token->src dot))))]
             [else (set! c #f)]))
    expr)
  (define (primary)
    (cond
      [(match 'FALSE) (datum->syntax #f `(lox-literal ,#f) (token->src (previous)))]
      [(match 'TRUE) (datum->syntax #f `(lox-literal ,#t) (token->src (previous)))]
      [(match 'NIL) (datum->syntax #f #'lox-nil (token->src (previous)))]
      [(match 'STRING)
       (datum->syntax #f `(lox-literal ,(token-lexeme (previous))) (token->src (previous)))]
      [(match 'NUMBER)
       (datum->syntax #f `(lox-literal ,(token-literal (previous))) (token->src (previous)))]
      [(match 'SUPER)
       (let ([keyword (previous)])
         (consume 'DOT "Expect '.' after 'super'.")
         (define method (consume 'IDENTIFIER "Expect superclass method name."))
         (datum->syntax #f `(lox-super ,keyword ,method) (token->src keyword)))]
      [(match 'THIS) (datum->syntax #f `(lox-this ,(previous)) (token->src (previous)))]
      [(match 'IDENTIFIER)
       (datum->syntax #f `(lox-variable ,(token->symbol (previous))) (token->src (previous)))]
      [(match 'LEFT_PAREN)
       (let ([paren (previous)]
             [expr (expression)])
         (consume 'RIGHT_PAREN "Expect ')' after expression")
         (datum->syntax #f `(lox-grouping ,expr) (token->src paren)))]
      [else (parse-error (peek) "Expect expression.")]))
  (define (print-statement)
    (define print-token (previous))
    (define value (expression))
    (consume 'SEMICOLON "Expect ';' after value.")
    (datum->syntax #f `(lox-print ,value) (token->src print-token)))
  (define (assignment)
    (define expression (or-syntax))
    (when (match 'EQUAL)
      (define equal (previous))
      (define value (assignment))
      (with-syntax ([value value])
        (set! expression
              (syntax-parse expression
                #:datum-literals (lox-variable lox-get)
                [(lox-variable name:expr)
                 ;; reuse expression’s source location
                 (syntax/loc expression
                   (lox-assign name value))]
                [(lox-get obj:expr name:expr)
                 (syntax/loc expression
                   (lox-set obj name value))]
                [_ (parse-error equal "Invalid assignment target.")]))))
    expression)
  (define-syntax-rule (iterative-production name production . token-types)
    (define (name)
      (define expr (production))
      (while (match .
               token-types)
             (define op (previous))
             (define right (production))
             (define op-type (token-type op))
             (set! expr (datum->syntax #f `(lox-binary ,expr ,op-type ,right) (token->src op))))
      expr))
  (iterative-production factor unary 'SLASH 'STAR)
  (iterative-production term factor 'MINUS 'PLUS)
  (iterative-production or-syntax and-syntax 'OR)
  (iterative-production and-syntax equality 'AND)
  (iterative-production equality comparison 'BANG_EQUAL 'EQUAL_EQUAL)
  (iterative-production comparison term 'GREATER 'GREATER_EQUAL 'LESS 'LESS_EQUAL)
  (define (unary)
    (if (not (match 'BANG
               'MINUS))
        (call)
        (let ([op (previous)])
          (define right (unary))
          (datum->syntax #f `(lox-unary ,(token-type op) ,right) (token->src op)))))
  (define (expression)
    (assignment))
  (define (statement)
    (cond
      [(match 'FOR) (for-statement)]
      [(match 'IF) (if-statement)]
      [(match 'PRINT) (print-statement)]
      [(match 'RETURN) (return-statement)]
      [(match 'WHILE) (while-statement)]
      [(match 'LEFT_BRACE) (block-statement)]
      [else (expression-statement)]))
  ;(trace block declaration block-statement statement for-statement var-declaration assignment print-statement expression or-syntax and-syntax factor unary term comparison equality call primary finish-call)
  (define (protected-declaration)
    (with-handlers ([exn:fail:lox? (lambda (e)
                                     (set! _hadError #t)
                                     (synchronize)
                                     (displayln (exn-message e) (current-error-port))
                                     #'null)])
      (declaration)))
  (define statements
    (if (is-at-end?)
        null
        (for/list ([decl (in-producer protected-declaration)]
                   #:final (is-at-end?)
                   #:when (lambda (el) (not (null? el))))
          decl)))
  (when _hadError
    (exit 65))
  statements)
(provide parse)

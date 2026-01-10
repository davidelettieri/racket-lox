#lang racket

(require "helpers.rkt" "scanner.rkt")
(require syntax/parse racket/trace)

(define (token->src t)
    (token-srcloc t))

(define (token->symbol t)
    (datum->syntax
        #f
        (string->symbol (token-lexeme t))
        (token->src t)))

(define (parse tokens)
    (define _tokens (list->vector tokens))
    (define _current 0)
    (define (advance)
        (when (not (is-at-end?)) (set! _current (+ _current 1)))
        (previous))
    (define (consume type [message ""])
        (if (check type)
            (advance)
            (let* ([tok (peek)]
                   [msg (if (string=? message "")
                            (format "Expect ~a." type)
                            message)]
                   [stx (datum->syntax #f (token-lexeme tok) (token->src tok))])
              (raise-syntax-error 'parse msg stx))))
    (define (check type)
        (and (not (is-at-end?)) (eqv? (token-type (peek)) type)))
    (define (check-next type)
        (and 
            (not is-at-end?)
            (not (eqv? (token-type (vector-ref _tokens (+ _current 1)) 'EOF)))
            (eqv? (token-type (vector-ref _tokens (+ _current 1)) type))))
    (define (previous) (vector-ref _tokens (- _current 1)))
    (define (peek) (vector-ref _tokens _current))
    (define (is-at-end?)
        (eqv? (token-type (peek)) 'EOF))
    (define (match . token-types)
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
            (set! superclass (datum->syntax
                #f
                (string->symbol (token-lexeme superclassToken))
                (token->src superclassToken))))
        (consume 'LEFT_BRACE "Expect '{' before class body.")
        (define methods '())
        (while (not (and (check 'RIGHT_BRACE) (not (is-at-end?))))
            (set! methods (cons (function "method") methods)))
        (consume 'RIGHT_BRACE "Expect '}' after class body.")
        (define name-id (token->symbol name))
        (datum->syntax #f `(lox-class ,name-id ,superclass ,methods)))
    (define (function kind)
        (define name (consume 'IDENTIFIER (format "Expect ~a name." kind)))
        (consume 'LEFT_PAREN (format "Expect '(' after ~a name." kind))
        (define parameters '())
        (when (not (check 'RIGHT_PAREN))
            (do 
                (when (> (length parameters) 256)
                    (raise "Too many parameters"))
                (set! parameters (cons (consume 'IDENTIFIER "Expect parameter name") parameters))
            while
                (match 'COMMA)))
        (consume 'RIGHT_PAREN "Expect ')' after parameters.")
        (consume 'LEFT_BRACE (format "Expect '{' before ~a body" kind))
        (define body (block))
        (define name-id (token->symbol name))
        (datum->syntax #f `(lox-function ,name-id ,parameters ,body)))
    (define (var-declaration)
        (define name (consume 'IDENTIFIER "Expect variable name."))
        (define initializer
            (if (match 'EQUAL) (expression) #f))
        (consume 'SEMICOLON "Expect ';' after variable declaration.")
        (define name-id (token->symbol name))
        (datum->syntax #f `(lox-var-declaration ,name-id ,initializer)))
    (define (block)
        (define statements (for/list (
            [decl (in-producer declaration)]
            #:break (or (check 'RIGHT_BRACE) (is-at-end?))
            #:when (lambda (el) (not (null? el))))
            decl))
        (consume 'RIGHT_BRACE "Expect '}' after block.")
        statements)
    (define (for-statement) (error 'for-statement-not-implemented))
    (define (if-statement)
        (consume 'LEFT_PAREN "Expect '(' after if.")
        (define expr (expression))
        (consume 'RIGHT_PAREN "Expect ')' after if.")
        (define then (statement))
        (if (match 'ELSE)
            (datum->syntax #f `(lox-if ,expr ,then ,(statement)))
            (datum->syntax #f `(lox-if ,expr ,then))))
    (define (return-statement) (error 'return-statement-not-implemented))
    (define (while-statement)
        (consume 'LEFT_PAREN "Expect '(' after 'while'.")
        (define expr (expression))
        (consume 'RIGHT_PAREN "Expect ')' after condition.")
        (define stmt (statement))
        (datum->syntax #f `(lox-while ,expr ,stmt)))
    (define (block-statement) (error 'block-statement-not-implemented))
    (define (expression-statement)
        (define value (expression))
        (consume 'SEMICOLON "Expect ';' after expression.")
        value)
    (define (finish-call) (error 'finish-call-not-implemented))
    (define (call)
        (define expr (primary))
        (define c #t)
        (while c
            (cond 
                [(match 'LEFT_PAREN) (set! expr (finish-call expr))]
                [(match 'DOT) (begin
                                (define name (consume 'IDENTIFIER "Expect property name after '.'."))
                                (set! expr 
                                    (datum->syntax #f `(lox-get ,expr ,name))))]
                [else (set! c #f)]))
        expr)
    (define (primary)
        (cond
            [(match 'FALSE) (datum->syntax #f `(lox-literal ,#f) (token->src (previous)))]
            [(match 'TRUE) (datum->syntax #f `(lox-literal ,#t) (token->src (previous)))]
            [(match 'NIL) (datum->syntax #f #'(lox-nil) (token->src (previous)))]
            [(match 'STRING) (datum->syntax #f `(lox-literal ,(token-lexeme (previous))) (token->src (previous)))]
            [(match 'NUMBER) (datum->syntax #f `(lox-literal ,(token-literal (previous))) (token->src (previous)))]
            [(match 'SUPER) 
                (let ([keyword (previous)])
                    (consume 'DOT "Expect '.' after 'super'.")
                    (define method (consume 'IDENTIFIER "Expect superclass method name."))
                    (datum->syntax #f `(lox-super ,keyword ,method)))]
            [(match 'THIS) (datum->syntax #f `(lox-this ,(previous)))]
            [(match 'IDENTIFIER) (datum->syntax #f `(lox-variable ,(token->symbol (previous))) (token->src (previous)))]
            [(match 'LEFT_PAREN) (let ([expr (expression)])
                (consume 'RIGHT_PAREN "Expect ')' after expression")
                (datum->syntax #f `(lox-grouping ,expr)))]
            [else (let* ([tok (peek)]
                         [stx (datum->syntax #f (token-lexeme tok) (token->src tok))]) 
                         (raise-syntax-error 'parse "Expect expression." stx))]))
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
                (set! expression (syntax-parse expression
                    #:datum-literals (lox-variable lox-get)
                    [(lox-variable name:expr)
                        ;; reuse expression’s source location
                        (syntax/loc expression (lox-assign name value))]
                    [(lox-get obj:expr name:expr)
                        (syntax/loc expression (lox-set obj name value))]
                    [_ (raise-syntax-error 'parse "Invalid assignment target" equal)]))))
        expression)
    (define-syntax-rule (iterative-production name production . token-types)
        (define (name)
            (define expr (production))
            (while (match . token-types)
                (define op (previous))
                (define right (production))
                (define op-name (token-lexeme op))
                (set! expr (datum->syntax #f `(lox-binary ,expr ,op-name ,right))))
            expr))
    (iterative-production factor unary 'SLASH 'STAR)
    (iterative-production term factor 'MINUS 'PLUS)
    (iterative-production or-syntax and-syntax 'OR)
    (iterative-production and-syntax equality 'AND)
    (iterative-production equality comparison 'BANG_EQUAL 'EQUAL_EQUAL)
    (iterative-production comparison term 'GREATER 'GREATER_EQUAL 'LESS 'LESS_EQUAL)
    (define (unary)
        (if (not (match 'BANG 'MINUS))
            (call)
            (let ([op (previous)]) 
                (define right (unary))
                (datum->syntax #f `(lox-unary ,op ,right)))))
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
    ;(trace declaration var-declaration assignment print-statement expression or-syntax and-syntax factor unary term comparison equality call primary)
    (for/list (
        [decl (in-producer declaration)]
        #:final (is-at-end?)
        #:when (lambda (el) (not (null? el))))
        decl))
(provide parse)
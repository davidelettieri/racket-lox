#lang racket

(require "helpers.rkt" "scanner.rkt")

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
                (when (> (length parameters) 0)
                    (raise "Too many parameters"))
                (set! parameters (cons (consume 'IDENTIFIER "Expect parameter name") parameters))
            while
                (match 'COMMA)))
        (consume 'RIGHT_PAREN "Expect ')' after parameters.")
        (consume 'LEFT_BRACE (format "Expect '{' before ~a body" kind))
        (define body (block))
        (define name-id (token->symbol name))
        (datum->syntax #f `(lox-function ,name-id ,parameters ,body)))
    (define (var-declaration) (error 'not-implemented))
    (define (block) (error 'not-implemented))
    (define (for-statement) (error 'not-implemented))
    (define (if-statement) (error 'not-implemented))
    (define (return-statement) (error 'not-implemented))
    (define (while-statement) (error 'not-implemented))
    (define (block-statement) (error 'not-implemented))
    (define (expression-statement) (error 'not-implemented))
    (define (print-statement)
        (define print-token (previous))
        (define value (expression))
        (consume 'SEMICOLON "Expect ';' after value.")
        (datum->syntax #f `(lox-print ,value) (token->src print-token)))
    (define (expression)
        (define t (advance))
        (datum->syntax #f (token-literal t) (token->src t)))
    (define (statement)
        (cond
            [(match 'FOR) (for-statement)]
            [(match 'IF) (if-statement)]
            [(match 'PRINT) (print-statement)]
            [(match 'RETURN) (return-statement)]
            [(match 'WHILE) (while-statement)]
            [(match 'LEFT_BRACE) (block-statement)]
            [else (expression-statement)]))
    (define statements '())
    (while (not (is-at-end?))
        (define decl (declaration))
        (when (not (null? decl))
            (set! statements (cons decl statements))))
    (reverse statements))

(provide parse)
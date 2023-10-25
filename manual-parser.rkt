#lang racket

(require parser-tools/lex racket/trace)

(define _tokens #f)
(define statements '())
(define _current 0)

(define (positions->srcloc start-pos end-pos)
  (srcloc #f
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)
          (- (position-offset end-pos) (position-offset start-pos))))

(define (position-token->syntax val start-token end-token)
  (let ([start-pos (position-token-start-pos start-token)]
        [end-pos (position-token-end-pos end-token)])
  (datum->syntax #f val (positions->srcloc start-pos end-pos))))


(define (parse-impl)
  (if (is-at-end) 
    (void) 
    (set! statements (cons (primary) statements))))

(define (declaration)
  (cond
    [(match? 'CLASS) (classDeclaration)]
    [(and (check? 'FUN) (check-next? 'IDENTIFIER)) (begin (consume 'FUN) (function "function"))]
    [(match? 'VAR) (varDeclaration)]
    [else (statement)]))

(define (classDeclaration) #'lox-class-declaration)
(define (function kind) #`(lox-function ,kind))

(define (varDeclaration)
  (define start-token (previous))
  (define name (consume 'IDENTIFIER "Expect variable name"))
  (define initializer
    (if (match? 'EQUAL)
      (expression)
      `lox-nil))
  (define end-token (consume 'SEMICOLON "Expect ';' after variable declaration."))
  (position-token->syntax `(lox-define-var ,name ,initializer) start-token end-token))

(define (expression)
  (assignment))

(define (assignment)
  (define start-token (previous))
  (if (match? 'EQUAL)
    (let ((value (assignment)))
             (position-token->syntax `(lox-define-var ,start-token ,value)))
      start-token))

(define (statement)
  (cond
    [(match? 'FOR) `(lox-for-statement)]
    [(match? 'IF) `(lox-if-statements)]
    [(match? 'PRINT) (print-statement)]
    [(match? 'RETURN) `(lox-return-statement)]
    [(match? 'WHILE) `(lox-while-statement)]
    [(match? 'LEFT_BRACE) `(lox-block)]
    [else `(lox-expression-statement)]))

(define (print-statement)
  (define start-token (previous))
  (define value (expression))
  (define end-token (consume 'SEMICOLON "Expect ';' after value."))
  (position-token->syntax `(lox-print ,value) start-token end-token))

(define (primary)
  (cond
    [(match? 'NUMBER) (let ((token (previous))) 
                        (position-token->syntax `(lox-number ,(token-value (position-token-token token))) token token))]))

(define (parse tokens)
  (set! _tokens tokens)
  (parse-impl)
  `#(lox-program ,statements))

(define (match? types)
  (cond
    [(not (list? types)) (if (check? types) (begin (advance) #t) #f)]
    [(empty? types) #f]
    [(check? (car types)) (begin (advance) #t)]
    [else (match? (cdr types))]))

(define (consume token-type message)
  (if (check? token-type)
    (advance)
    (raise token-type)))

(define (check? token-type)
  (if (is-at-end)
    #f
    (eqv-token-type? (peek) token-type)))

(define (check-next? token-type)
  (cond
    [(is-at-end) #f]
    [(is-eof? (list-ref _tokens (+ _current 1))) #f]
    [else (eqv-token-type? (list-ref _tokens (+ _current 1)) token-type)]))

(define (advance)
  (if (is-at-end)
    (previous)
    (set! _current (+ _current 1))))

(define (is-at-end)
  (is-eof? (peek)))

(define (peek)
  (list-ref _tokens _current))

(define (previous)
  (list-ref _tokens (- _current 1)))

(define (is-eof? t)
  (eqv-token-type? t 'EOF))

(define (eqv-token-type? position-token type)
  (define token (position-token-token position-token))
  (define tt (if (token? token) (token-name token) token))
  (eqv? tt type)) 

; (trace match? check? varDeclaration assignment expression position-token->syntax)

(provide parse eqv-token-type?)
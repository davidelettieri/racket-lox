#lang racket

(require parser-tools/lex racket/trace)

(define _tokens #f)
(define statements '())
(define _current 0)
(define _length 0)

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

(define-syntax get-syntax-2
  (syntax-rules ()
    [(_ node tvalue1 tvalue2 tstart tend) 
      (let ([value2 (if (position-token? tvalue2) (token-value (position-token-token tvalue2)) tvalue2)]
            [value1 (if (position-token? tvalue1) (token-value (position-token-token tvalue1)) tvalue1)]
            [start (position-token-start-pos tstart)]
            [end (position-token-end-pos tend)])
        (datum->syntax #f `(node ,value1 ,value2) (positions->srcloc start end)))]))

(define-syntax get-syntax-1 
  (syntax-rules ()
    [(_ node tvalue) (get-syntax-1 node tvalue tvalue tvalue)]
    [(_ node tvalue tend) (get-syntax-1 node tvalue tvalue tend)]
    [(_ node tvalue tstart tend) 
      (let ([value (if (position-token? tvalue) (token-value (position-token-token tvalue)) tvalue)]
            [start (position-token-start-pos tstart)]
            [end (position-token-end-pos tend)])
        (datum->syntax #f `(node ,value) (positions->srcloc start end)))]))

(define-syntax get-syntax-0
  (syntax-rules ()
    [(_ node token) 
      (let ([start (position-token-start-pos token)]
            [end (position-token-end-pos token)])
        (datum->syntax #f `node (positions->srcloc start end)))]))

(define (parse-impl)
  (if (is-at-end) 
    (void) 
    (set! statements (cons (assignment) statements))))

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
  (primary))

(define (assignment)
  (define expr (primary))
  (displayln expr)
  (if (match? 'EQUAL)
    (let ((start (previous))
          (a (assignment))
          (end (previous)))
      (get-syntax-2 lox-assignment expr a start end))
      expr))

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
    [(match? 'NUMBER) (get-syntax-1 lox-number (previous))]
    [(match? 'STRING) (get-syntax-1 lox-string (previous))]
    [(match? 'TRUE) (get-syntax-1 #t (previous))]
    [(match? 'FALSE) (get-syntax-1 #f (previous))]
    [(match? 'NIL) (get-syntax-0 lox-nil (previous))]
    [(match? 'LEFT_PAREN) (begin
                            (define start (previous))
                            (define value (expression))
                            (define end (consume 'RIGHT_PAREN "dadsada"))
                            (get-syntax-1 lox-grouping value start end))]
    [(match? 'IDENTIFIER) (get-syntax-1 lox-var-value (previous))]))

(define (parse tokens)
  (set! _tokens tokens)
  (set! _length (length tokens))
  (parse-impl)
  `(lox-program ,statements))

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
  (unless (is-at-end)
    (set! _current (+ _current 1)))
  (previous))

(define (is-at-end)
  (>= _current _length))

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

; (trace match? )
(provide parse eqv-token-type?)
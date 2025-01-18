#lang racket

(require parser-tools/yacc
         parser-tools/lex
         racket/match
         "lexer.rkt")

; (struct srcloc (source line column position span))
(define (positions->srcloc start-pos end-pos)
  (srcloc #f
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)
          (- (position-offset end-pos) (position-offset start-pos))))

; docs for datum->syntax
; https://docs.racket-lang.org/reference/stxops.html#%28def._%28%28quote._~23~25kernel%29._datum-~3esyntax%29%29
(define (position-token->syntax val start-pos end-pos)
  (datum->syntax #f val (positions->srcloc start-pos end-pos)))

(define (get-tokens l)
  (define (get-tokens-inner source)
    (let [(t (l))] (if (eof-object? t) source (cons t (get-tokens-inner)))))
  (reverse (get-tokens-inner '())))

(define (pretty-print-token t)
  (match t
    ['THIS "this"]
    ['NIL "nil"]
    ['FALSE "false"]))

; (lambda (tok-ok? tok-name tok-value start-pos end-pos)
(define (raise-parse-error tok-ok? tok-name tok-value start-pos end-pos #:stack stack)
  (let [(s (car (car stack)))]
    (cond
      [(not tok-ok?) (raise-user-error "unexpected token")]
      [(eqv? s 32) (raise-user-error (format "[line ~a] Error at '~a': Expect variable name." (position-line start-pos) (pretty-print-token tok-name)))]
      [(eqv? tok-name 'EQUAL) (raise-user-error (format "[line ~a] Error at '=': Invalid assignment target." (position-line start-pos)))]
      [(eqv? tok-name 'DOT) (raise-user-error (format "[line ~a] Error at '.': Expect expression." (position-line start-pos)))]
      [(eqv? tok-name 'SEMICOLON) (raise-user-error (format "[line ~a] Error at ';': Expect expression." (position-line start-pos)))]
      [(and (eqv? tok-name 'LEXER_ERROR) (equal? tok-value "\"")) (raise-user-error (format "[line ~a] Error: Unterminated string." (position-line start-pos)))]
      [else (begin (println (list tok-ok? tok-name tok-value start-pos end-pos (car stack))) (raise-user-error "t"))])))

(define lox-parser
  (parser
   [start program]
   [end EOF]
   [error raise-parse-error]
   [src-pos]
   [tokens basic-tokens punct-tokens]
   ; precs clause go from lowest to highest priority
   ; tokens at the same level share the same priority
   [precs (left PLUS MINUS)
          (left STAR SLASH)
          (nonassoc RIGHT_PAREN)
          (nonassoc ELSE)]
  ;  [debug "debug.log"]
  ;  [yacc-output "yacc.output.log"]
   [grammar
    [program [() #'(lox-empty-program)]
             [(declarations) $1]
             [(error declarations) $2]]
    [declaration [(varDecl) $1]
                 [(statement) $1]]
    [varDecl [(VAR IDENTIFIER SEMICOLON) (position-token->syntax `(lox-define-var ,$2 lox-nil) $1-start-pos $3-end-pos)]
             [(VAR IDENTIFIER EQUAL expression SEMICOLON) (position-token->syntax `(lox-define-var ,$2 ,$4) $1-start-pos $5-end-pos)]]
    [statement [(exprStmt) $1]
               [(ifStmt) $1]
               [(printStmt) $1]
               [(block) $1]]
    [ifStmt [(IF LEFT_PAREN expression RIGHT_PAREN statement) (position-token->syntax `(lox-if ,$3 ,$5) $1-start-pos $5-end-pos)]
            [(IF LEFT_PAREN expression RIGHT_PAREN statement ELSE statement) (position-token->syntax `(lox-if ,$3 ,$5 ,$7) $1-start-pos $7-end-pos)]]
    [block [(LEFT_BRACE declarations RIGHT_BRACE) (position-token->syntax `(lox-block ,$2) $1-start-pos $3-end-pos)]]
    [declarations [(declaration) (position-token->syntax $1 $1-start-pos $1-end-pos)]
                  [(declaration declarations) (position-token->syntax `(lox-declarations ,$1 ,$2) $1-start-pos $2-end-pos)]]
    [exprStmt [(expression SEMICOLON) $1]]
    [printStmt [(PRINT expression SEMICOLON) (position-token->syntax `(lox-print ,$2) $1-start-pos $3-end-pos)]]
    ; expression     → equality ;
    [expression [(assignment) $1]]
    [assignment [(IDENTIFIER EQUAL assignment) (position-token->syntax `(lox-assignment ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(equality) $1]]
    ; ; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    [equality [(comparison BANG_EQUAL comparison) (position-token->syntax `(not (lox-eqv? ,$1 ,$3)) $1-start-pos $3-end-pos)]
              [(comparison EQUAL_EQUAL comparison) (position-token->syntax `(lox-eqv? ,$1 ,$3) $1-start-pos $3-end-pos)]
              [(comparison) $1]]
    ; ; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    [comparison [(term GREATER term) (position-token->syntax `(lox-greater ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term GREATER_EQUAL term) (position-token->syntax `(lox-greater-equal ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term LESS term) (position-token->syntax `(lox-less ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term LESS_EQUAL term) (position-token->syntax `(lox-less-equal ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term) $1]]
    ; ; term           → factor ( ( "-" | "+" ) factor )* ;
    [term [(factor PLUS term) (position-token->syntax `(lox-add ,$1 ,$3) $1-start-pos $3-end-pos)]
          [(factor MINUS term)(position-token->syntax `(lox-subtract ,$1 ,$3) $1-start-pos $3-end-pos)]
          [(factor) $1]]
    ; ; factor         → unary ( ( "/" | "*" ) unary )* ;
    [factor [(unary STAR unary) (position-token->syntax `(lox-multiply ,$1 ,$3) $1-start-pos $3-end-pos)]
            [(unary SLASH unary) (position-token->syntax `(lox-divide ,$1 ,$3) $1-start-pos $3-end-pos)]
            [(unary) $1]]
    ; ; unary → ( "!" | "-" ) unary | primary ;
    [unary [(BANG unary) (position-token->syntax `(not ,$2) $1-start-pos $2-end-pos)]
           [(MINUS unary) (position-token->syntax `(lox-negate ,$2) $1-start-pos $2-end-pos)]
           [(primary) $1]]
    ; primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    [primary [(NUMBER) (position-token->syntax `(lox-number ,$1) $1-start-pos $1-end-pos)]
             [(STRING) (position-token->syntax `(lox-string ,$1) $1-start-pos $1-end-pos)]
             [(TRUE) (position-token->syntax #t $1-start-pos $1-end-pos)]
             [(FALSE) (position-token->syntax #f $1-start-pos $1-end-pos)]
             [(NIL) (position-token->syntax `lox-nil $1-start-pos $1-end-pos)]
             [(LEFT_PAREN expression RIGHT_PAREN) (position-token->syntax $2 $1-start-pos $3-end-pos)]
             [(IDENTIFIER) (position-token->syntax `(lox-var-value ,$1) $1-start-pos $1-end-pos)]]]))

(provide lox-parser get-tokens)
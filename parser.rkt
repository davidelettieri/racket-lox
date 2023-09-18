#lang racket

(require parser-tools/yacc
         parser-tools/lex
         "lexer.rkt")


(define-syntax-rule (while test body ...)
  (local [(define (loop)
            (when test
              body ...
              (loop)))]
    (loop)))

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

(define lox-parser
  (parser
   [start program]
   [end EOF]
   (error (lambda (a t v start end) 
            (displayln (list a t v start end))))
   [src-pos]
   [tokens basic-tokens punct-tokens]
   ; precs clause go from lowest to highest priority
   ; tokens at the same level share the same priority
   [precs (left PLUS MINUS)
          (left STAR SLASH)]
  ;  [debug "debug.log"]
  ;  [yacc-output "yacc.output.log"]
   [grammar
    [program [(expression) $1]
             [(expression program) (position-token->syntax `(cons ,$1 ,$2) $1-start-pos $2-end-pos)]]
    ; expression     → equality ;
    [expression [(equality) $1]]
    ; ; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    [equality [(comparison BANG_EQUAL comparison) (position-token->syntax `(not= ,$1 ,$3) $1-start-pos $3-end-pos)]
              [(comparison EQUAL_EQUAL comparison) (position-token->syntax `(= ,$1 ,$3) $1-start-pos $3-end-pos)]
              [(comparison) $1]]
    ; ; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    [comparison [(term GREATER term) (position-token->syntax `(> ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term GREATER_EQUAL term) (position-token->syntax `(>= ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term LESS term) (position-token->syntax `(< ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term LESS_EQUAL term) (position-token->syntax `(<= ,$1 ,$3) $1-start-pos $3-end-pos)]
                [(term) $1]]
    ; ; term           → factor ( ( "-" | "+" ) factor )* ;
    [term [(factor PLUS factor) (position-token->syntax `(+ ,$1 ,$3) $1-start-pos $3-end-pos)]
          [(factor MINUS factor)(position-token->syntax `(- ,$1 ,$3) $1-start-pos $3-end-pos)]
          [(factor) $1]]
    ; ; factor         → unary ( ( "/" | "*" ) unary )* ;
    [factor [(unary STAR unary) (position-token->syntax `(* ,$1 ,$3) $1-start-pos $3-end-pos)]
            [(unary SLASH unary) (position-token->syntax `(/ ,$1 ,$3) $1-start-pos $3-end-pos)]
            [(unary) $1]]
    ; ; unary → ( "!" | "-" ) unary | primary ;
    [unary [(BANG unary) (position-token->syntax `(not ,$2) $1-start-pos $2-end-pos)]
           [(primary) $1]]
    ; primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    [primary [(NUMBER) (position-token->syntax $1 $1-start-pos $1-end-pos)]
             [(TRUE) (position-token->syntax #t $1-start-pos $1-end-pos)]
             [(FALSE) (position-token->syntax #f $1-start-pos $1-end-pos)]
             [(NIL) (position-token->syntax #f $1-start-pos $1-end-pos)]
             [(LEFT_PAREN expression RIGHT_PAREN) (position-token->syntax $2 $1-start-pos $3-end-pos)]
             [(IDENTIFIER) (position-token->syntax $1 $1-start-pos $1-end-pos)]]
    ]))

(provide lox-parser get-tokens)

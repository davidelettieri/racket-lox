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

(define (lox-parser tokens)
  (define current 0)
  (define (declaration) #f)
  (define (peek) (list-ref current tokens))
  (define (is-at-end?) (eof-object? (peek)))
  (define (parse)
    (define statements '())
    (while (not (is-at-end?))
      (set! statements (cons (declaration) statements)))
    statements)
  (parse))

(provide lox-parser get-tokens)


; (define the-parser
;   (parser
;    [start program]
;    [end EOF]
;    [error void]
;    [src-pos]
;    [tokens basic-tokens punct-tokens]
;    ; precs clause go from lowest to highest priority
;    ; tokens at the same level share the same priority
;    [precs (left PLUS MINUS)
;           (left STAR SLASH)]
;    [grammar
;     [program [() '()]
;              [(declaration program) (cons $1 $2)]]
;     [declaration [(classDecl) $1]]
;                  [(funDecl) $1]
;                  [(varDecl) $1]
;                  [(statement) $1]
;     [functions [() '()]
;                [(function functions) (cons $1 $2)]]
;     [classDecl [(CLASS IDENTIFIER LEFT_BRACE functions RIGHT_BRACE)]]
;           ; the start position of a parenthesized expression is the start position of the open '('
;     [expr [(LEFT_PAREN expr RIGHT_PAREN) (position-token->syntax $2 $1-start-pos $3-end-pos)] 
;           ; the start position of a number is the start position of the number token
;           [(NUM) (position-token->syntax $1 $1-start-pos $1-end-pos)]
;           ; the start position of all binary expression is the start position of the first expression
;           ; the end position is the end position of the second expression
;           [(expr STAR expr) (position-token->syntax `(multiply ,$1 ,$3) $1-start-pos $3-end-pos)]
;           [(expr SLASH expr) (position-token->syntax `(divide ,$1 ,$3) $1-start-pos $3-end-pos)]
;           [(expr MINUS expr) (position-token->syntax `(subtract ,$1 ,$3) $1-start-pos $3-end-pos)]
;           [(expr PLUS expr) (position-token->syntax `(add ,$1 ,$3) $1-start-pos $3-end-pos)]]
;     ]))

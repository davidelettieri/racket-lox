#lang racket

(require parser-tools/lex
        "lexer.rkt"
        "parser.rkt"
        "semantics.rkt"
        "manual-parser.rkt")

(define (parse-string s)
(let ([in (open-input-string s)])
    (lox-parser (lambda () (lox-lexer in)))))

(define (parse-string-manual s)
(let ([in (open-input-string s)])
    (define tokens (get-tokens (lambda () (lox-lexer in))))
    ; (displayln tokens)
    (parse tokens)))
    
(parse-string "a = 1;")
(parse-string-manual "a = 1")

; (define (main) (lox-program ((lox-number 1))) (void))
; (expand-once (lox-program ((lox-number 1))))

#lang racket

(define keywords 
    (hasheqv 
        "and" 'AND 
        "class" 'CLASS
        "else" 'ELSE
        "false" 'FALSE
        "for" 'FOR
        "fun" 'FUN
        "if" 'IF
        "nul" 'NIL
        "or" 'OR
        "print" 'PRINT
        "return" 'RETURN
        "super" 'SUPER
        "this" 'THIS
        "true" 'TRUE
        "var" 'VAR
        "while" 'WHILE))

(define (scan-tokens input-port)
    (port-count-lines! input-port)
    (scan-tokens-impl input-port '()))

(define (scan-tokens-impl input-port acc)
    (let [(current-char (peek-char input-port))]
        (if (eof-object? current-char)
            (reverse (cons current-char acc))
            (scan-tokens-impl input-port (cons (read-token input-port) acc)))))

(define (scan-token input-port)
    (let [(c (read-char input-port))
          (line (input-port-line input-port))
          (column (input-port-column input-port))]
        (case c
            [(#\() (token 'LEFT_PAREN #\( #f line column)])))

(struct token (type lexeme literal line column) #:transparent)

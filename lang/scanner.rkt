#lang racket

(require "helpers.rkt")

(provide
  scan-tokens
  scan-token
  (struct-out token))

(define keywords 
    (hasheqv 
        "and" 'AND 
        "class" 'CLASS
        "else" 'ELSE
        "false" 'FALSE
        "for" 'FOR
        "fun" 'FUN
        "if" 'IF
        "nil" 'NIL
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
    (let [(token (scan-token input-port))]
        (if (eqv? 'EOF (token-type token))
            (reverse (cons token acc))
            (scan-tokens-impl input-port (cons token acc)))))

(define (scan-token input-port)
    (define-values (line col pos) (port-next-location input-port))
    (let ([c (read-char input-port)])
        (if (eof-object? c) 
            (token 'EOF eof #f line col)
            (case c
                [(#\() (token 'LEFT_PAREN  #\( #f line col)]
                [(#\)) (token 'RIGHT_PAREN #\) #f line col)]
                [(#\{) (token 'LEFT_BRACE  #\{ #f line col)]
                [(#\}) (token 'RIGHT_BRACE #\} #f line col)]
                [(#\,) (token 'COMMA       #\, #f line col)]
                [(#\.) (token 'DOT         #\. #f line col)]
                [(#\-) (token 'MINUS       #\- #f line col)]
                [(#\+) (token 'PLUS        #\+ #f line col)]
                [(#\;) (token 'SEMICOLON   #\; #f line col)]
                [(#\*) (token 'STAR        #\* #f line col)]
                [(#\!) (if (match input-port #\=)
                           (token 'BANG_EQUAL "!=" #f line col)
                           (token 'BANG       #\!  #f line col))]
                [(#\=) (if (match input-port #\=)
                           (token 'EQUAL_EQUAL "==" #f line col)
                           (token 'EQUAL       #\=  #f line col))]
                [(#\<) (if (match input-port #\=)
                           (token 'LESS_EQUAL "<=" #f line col)
                           (token 'LESS       #\<  #f line col))]
                [(#\>) (if (match input-port #\=)
                           (token 'GREATER_EQUAL ">=" #f line col)
                           (token 'GREATER       #\>  #f line col))]
                [(#\/) (handle-slash input-port line col)]
                ;; Ignore simple whitespace and read the next token
                [(#\space #\tab #\newline #\return)
                    (scan-token input-port)]
                [else
                    (error 'scan-token (format "Unexpected character: ~a at ~a:~a"
                                            c line col))]))))

(define (handle-slash input-port line col)
    (if (match input-port #\/)
        (begin 
            (while 
                (and 
                    (not (eqv? (peek-char input-port) #\newline))
                    (not (is-at-end? input-port))) (read-char input-port))
            (scan-token input-port))
        (token 'SLASH #\/ #f line col)))

(define (is-at-end? input-port) (eof-object? (peek-char input-port)))

(define (match input-port expected)
    (if (eqv? (peek-char input-port) expected)
        (begin 
            (read-char input-port)
            #t)
        #f))

(struct token (type lexeme literal line column) #:transparent)
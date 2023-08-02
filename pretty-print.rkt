#lang racket/base

(require "lexer.rkt"
         parser-tools/lex)

(define (print token-type literal value)
  (fprintf (current-output-port) "~s ~a ~a\n" token-type literal value))

(define ht (hash
            'EOF ""
            'LEFT_PAREN "("
            'RIGHT_PAREN ")"
            'LEFT_BRACE "{"
            'RIGHT_BRACE "}"
            'COMMA ","
            'DOT "."
            'MINUS "-"
            'PLUS "+"
            'SEMICOLON ";"
            'SLASH "/"
            'STAR "*"
            'BANG "!"
            'BANG_EQUAL "!="
            'EQUAL "="
            'EQUAL_EQUAL "=="
            'GREATER ">"
            'GREATER_EQUAL ">="
            'LESS "<"
            'LESS_EQUAL "<="
            'AND "and"
            'CLASS "class"
            'ELSE "else"
            'FALSE "false"
            'FUN "fun"
            'FOR "for"
            'IF "if"
            'NIL "nil"
            'OR "or"
            'PRINT "print"
            'RETURN "return"
            'SUPER "super"
            'THIS "this"
            'TRUE "true"
            'VAR "var"
            'WHILE "while"))

(define (pretty-print pt)
  (begin
    (define token (position-token-token pt))
    (define name (token-name token))
    (define value (token-value token))
    (cond
      [(hash-has-key? ht name) (print name (hash-ref ht name) "null")]
      [(eq? 'NUMBER name) (print name value (+ 0.0 value))]
      [(eq? 'STRING name) (print name (string-append "\"" value "\"") value)] 
      [else (print name value "null")])))


(provide pretty-print)
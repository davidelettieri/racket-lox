#lang racket

(require "helpers.rkt"
         (for-syntax syntax/parse))

(provide
 scan-tokens
 (struct-out token))

(define keywords
  (hash
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
  (for/list (
    [token (in-producer 
      (lambda () (scan-token input-port)))]
      #:final (eqv? 'EOF (token-type token)))
      token))

;; helper to build srcloc with a real source
(define (make-src ip line col pos span)
  (srcloc (object-name ip) line col pos span))

(define (scan-token input-port)
  (define-values (line col pos) (port-next-location input-port))
  (let ([c (read-char input-port)])
    (if (eof-object? c)
        (token 'EOF eof #f (make-src input-port line col pos 1))
        (case c
          [(#\() (token 'LEFT_PAREN  #\( #f (make-src input-port line col pos 1))]
          [(#\)) (token 'RIGHT_PAREN #\) #f (make-src input-port line col pos 1))]
          [(#\{) (token 'LEFT_BRACE  #\{ #f (make-src input-port line col pos 1))]
          [(#\}) (token 'RIGHT_BRACE #\} #f (make-src input-port line col pos 1))]
          [(#\,) (token 'COMMA       #\, #f (make-src input-port line col pos 1))]
          [(#\.) (token 'DOT         #\. #f (make-src input-port line col pos 1))]
          [(#\-) (token 'MINUS       #\- #f (make-src input-port line col pos 1))]
          [(#\+) (token 'PLUS        #\+ #f (make-src input-port line col pos 1))]
          [(#\;) (token 'SEMICOLON   #\; #f (make-src input-port line col pos 1))]
          [(#\*) (token 'STAR        #\* #f (make-src input-port line col pos 1))]
          [(#\!) (if (match input-port #\=)
                     (token 'BANG_EQUAL "!=" #f (make-src input-port line col pos 2))
                     (token 'BANG       #\!  #f (make-src input-port line col pos 1)))]
          [(#\=) (if (match input-port #\=)
                     (token 'EQUAL_EQUAL "==" #f (make-src input-port line col pos 2))
                     (token 'EQUAL       #\=  #f (make-src input-port line col pos 1)))]
          [(#\<) (if (match input-port #\=)
                     (token 'LESS_EQUAL "<=" #f (make-src input-port line col pos 2))
                     (token 'LESS       #\<  #f (make-src input-port line col pos 1)))]
          [(#\>) (if (match input-port #\=)
                     (token 'GREATER_EQUAL ">=" #f (make-src input-port line col pos 2))
                     (token 'GREATER       #\>  #f (make-src input-port line col pos 1)))]
          [(#\/) (handle-slash input-port line col pos)]
          [(#\") (string-token input-port line col pos)]
          ;; Ignore simple whitespace and read the next token
          [(#\space #\tab #\newline #\return)
           (scan-token input-port)]
          [else
           (cond
             [(char-numeric? c) (number c input-port line col pos)]
             [(is-alphanumeric? c) (identifier c input-port line col pos)]
             [else
              (error 'scan-token (format "Unexpected character: ~a at ~a:~a"
                                         c line col))])]))))

;; update helpers to use make-src
(define (identifier c input-port line col pos)
  (define chars (list c))
  (define (advance)
    (define d (read-char input-port))
    (set! chars (cons d chars)))
  (while (let [(c (peek-char input-port))]
           (and (not (eof-object? c)) (is-alphanumeric? c)
                     (advance))))
  (define value (list->string (reverse chars)))
  (define type (hash-ref keywords value 'IDENTIFIER))
  (token type value #f (make-src input-port line col pos (string-length value))))

(define (number c input-port line col pos)
  (define chars (list c))
  (define (advance)
    (define d (read-char input-port))
    (set! chars (cons d chars)))
  (define (numeric? c)
    (and
     (not (eof-object? c))
     (char-numeric? c)))
  (while (numeric? (peek-char input-port))
         (advance))
  (when
      (and
       (eqv? (peek-char input-port) #\.)
       (numeric? (peek-char input-port 1)))
    (advance)
    (while
     (numeric? (peek-char input-port))
     (advance)))
  (define value (list->string (reverse chars)))
  (token 'NUMBER value (string->number (string-append "#i" value))
         (make-src input-port line col pos (string-length value))))

(define (string-token input-port line col pos)
  (define chars '())
  (while-not-char-not-end input-port #\" (set! chars (cons (read-char input-port) chars)))
  (when (is-at-end? input-port) (error "Unterminated string."))
  (read-char input-port)
  (define value (list->string (reverse chars)))
  (token 'STRING value #f
         (make-src input-port line col pos (+ 2 (string-length value)))))

(define (handle-slash input-port line col pos)
  (if (match input-port #\/)
      (begin
        (while-not-char-not-end input-port #\newline (read-char input-port))
        (scan-token input-port))
      (token 'SLASH #\/ #f (make-src input-port line col pos 1))))

(define (is-at-end? input-port) (eof-object? (peek-char input-port)))
(define (is-alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c) (eqv? c #\_)))

(define (match input-port expected)
  (if (eqv? (peek-char input-port) expected)
      (begin
        (read-char input-port)
        #t)
      #f))

(struct token (type lexeme literal srcloc) #:transparent)

(define-syntax (while-not-char-not-end stx)
  (syntax-parse stx
    [(_ input-port:id c:char body:expr ...)
     #'(while (and
               (not (eqv? (peek-char input-port) c))
               (not (is-at-end? input-port)))
              body ...)]))
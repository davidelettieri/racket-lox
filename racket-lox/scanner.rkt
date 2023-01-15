#lang racket
(require racket/trace)

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

(provide get-tokens token)

(struct token (type line column pos value) #:transparent)
(define (make-token type line col pos [value #f])
   (token type line col pos value))

(define (match sp ch type-1 type-2 line col pos)
    (if (eqv? (peek-char sp) ch)
        (begin
          (read-char sp)
          (make-token type-1 sp line col pos))
        (make-token type-2 sp line col pos)))

(define (comment sp)
  (begin
    (read-line sp)
    (get-token sp)))

(define (get-string sp line col pos source)
  (let ((next-c (peek-char sp)))
        (cond
          ((eof-object? next-c) (raise 'unterminated-string))
          ((eqv? next-c #\") (begin (read-char sp) (make-token 'STRING line col pos (list->string (reverse source)))))
          (else (begin (read-char sp) (get-string sp line col pos (cons next-c source)))))))

(define (get-number sp line col pos source)
  (let ((next-c (peek-char sp)) (next-cc (peek-char sp 1)))
        (cond
          ((eof-object? next-c) (make-token 'NUMBER line col pos (string->number (list->string (reverse source)))))
          ((char-numeric? next-c) (begin (read-char sp) (get-number sp line col pos (cons next-c source))))
          ((and (eqv? #\. next-c) (char-numeric? next-cc)) (begin (read-char sp) (get-number sp line col pos (cons next-c source))))
          (else (make-token 'NUMBER line col pos (string->number (list->string (reverse source))))))))
          
(define (get-identifier sp line col pos source)
  (let ((next-c (peek-char sp)))
    (if (and (not (eof-object? next-c)) (or (char-alphabetic? next-c) (char-numeric? next-c)))
        (let ((c (read-char sp)))
          (get-identifier sp line col pos (cons c source)))
        (let* ((text (list->string (reverse source)))
               (kw (hash-ref keywords text #f)))
          (if kw
              (make-token kw line col pos)
              (make-token 'IDENTIFIER line col pos text))))))
      
(define (get-token sp)
  (begin
    (define-values (line col pos) (port-next-location sp))
    (define c (read-char sp))
    (case c
      ((#\() (make-token 'LEFT_PAREN line col pos))
      ((#\)) (make-token 'RIGHT_PAREN line col pos))
      ((#\{) (make-token 'LEFT_BRACE line col pos))
      ((#\}) (make-token 'RIGHT_BRACE line col pos))
      ((#\,) (make-token 'COMMA line col pos))
      ((#\.) (make-token 'DOT line col pos))
      ((#\-) (make-token 'MINUS line col pos))
      ((#\+) (make-token 'PLUS line col pos))
      ((#\;) (make-token 'SEMICOLON line col pos))
      ((#\*) (make-token 'STAR line col pos))
      ((#\!) (match sp #\= 'BANG_EQUAL 'BANG line col pos))
      ((#\=) (match sp #\= 'EQUAL_EQUAL 'EQUAL line col pos))
      ((#\<) (match sp #\= 'LESS_EQUAL 'LESS line col pos))
      ((#\>) (match sp #\= 'GREATER_EQUAL 'GREATER line col pos))
      ((#\/) (if (eqv? (peek-char sp) #\/) (comment sp) (make-token 'SLASH line col pos)))
      ((#\newline) (get-token sp))
      ((#\tab) (get-token sp))
      ((#\return) (get-token sp))
      ((#\space) (get-token sp))
      ((#\") (get-string sp line col pos '()))
      (else (cond
            ((eof-object? c) (make-token 'EOF line col pos))
            ((char-numeric? c) (get-number sp line col pos (list c)))
            ((char-alphabetic? c) (get-identifier sp line col pos (list c)))
            (else raise 'unexpected-character))))))

(define (get-tokens-impl s tokens)
  (let* ((t (get-token s)) (next-s (cons t tokens)))
    (if (eqv? (token-type t) 'EOF) (reverse next-s) (get-tokens-impl s next-s))))

(define (get-tokens sp)
    (get-tokens-impl sp '()))

;(trace get-token get-tokens-impl)
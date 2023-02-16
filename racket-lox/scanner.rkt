#lang racket
(require racket/trace)

(define keywords
  (hash
   "and" '(AND . 3)
   "class" '(CLASS . 5)
   "else" '(ELSE . 4)
   "false" '(FALSE . 5)
   "for" '(FOR . 3)
   "fun" '(FUN . 3)
   "if" '(IF . 2)
   "nil" '(NIL . 3)
   "or" '(OR . 2)
   "print" '(PRINT . 5)
   "return" '(RETURN . 6)
   "super" '(SUPER . 5)
   "this" '(THIS . 4)
   "true" '(TRUE . 4)
   "var" '(VAR . 3)
   "while" '(WHILE . 5)))

(provide get-token get-tokens token)

(struct token (type line column pos span value) #:transparent)
(define (make-token type line col pos span [value #f])
   (token type line col pos span value))

(define (match sp ch type-1 type-2 line col pos span)
    (if (eqv? (peek-char sp) ch)
        (begin
          (read-char sp)
          (make-token type-1 sp line col pos span))
        (make-token type-2 sp line col pos span)))

(define (comment sp)
  (begin
    (read-line sp)
    (get-token sp)))

(define (get-string sp line col pos source)
  (let ((next-c (peek-char sp)))
        (cond
          ((eof-object? next-c) (raise 'unterminated-string))
          ((eqv? next-c #\") (begin
                               (read-char sp)
                               (define lexeme (list->string (reverse source)))
                               (make-token 'STRING line col pos (string-length lexeme) lexeme)))
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
              (make-token (car kw) line col pos (cdr kw))
              (make-token 'IDENTIFIER line col pos (string-length text) text))))))
      
(define (get-token sp)
  (begin
    (define-values (line col pos) (port-next-location sp))
    (define c (read-char sp))
    (case c
      ((#\() (make-token 'LEFT_PAREN line col pos 1))
      ((#\)) (make-token 'RIGHT_PAREN line col pos 1))
      ((#\{) (make-token 'LEFT_BRACE line col pos 1))
      ((#\}) (make-token 'RIGHT_BRACE line col pos 1))
      ((#\,) (make-token 'COMMA line col pos 1))
      ((#\.) (make-token 'DOT line col pos 1))
      ((#\-) (make-token 'MINUS line col pos 1))
      ((#\+) (make-token 'PLUS line col pos 1))
      ((#\;) (make-token 'SEMICOLON line col pos 1))
      ((#\*) (make-token 'STAR line col pos 1))
      ((#\!) (match sp #\= 'BANG_EQUAL 'BANG line col pos 2))
      ((#\=) (match sp #\= 'EQUAL_EQUAL 'EQUAL line col pos 2))
      ((#\<) (match sp #\= 'LESS_EQUAL 'LESS line col pos 2))
      ((#\>) (match sp #\= 'GREATER_EQUAL 'GREATER line col pos 2))
      ((#\/) (if (eqv? (peek-char sp) #\/) (comment sp) (make-token 'SLASH line col pos 1)))
      ((#\newline) (get-token sp))
      ((#\tab) (get-token sp))
      ((#\return) (get-token sp))
      ((#\space) (get-token sp))
      ((#\") (get-string sp line col pos '()))
      (else (cond
            ((eof-object? c) c)
            ((char-numeric? c) (get-number sp line col pos (list c)))
            ((char-alphabetic? c) (get-identifier sp line col pos (list c)))
            (else raise 'unexpected-character))))))

(define (get-tokens-impl s tokens)
  (let* ((t (get-token s)) (next-s (cons t tokens)))
    (if (eof-object? t) (reverse next-s) (get-tokens-impl s next-s))))

(define (get-tokens sp)
    (get-tokens-impl sp '()))

;(trace get-token get-tokens-impl)
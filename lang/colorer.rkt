#lang racket/base
(require racket/match
         racket/port
         "scanner.rkt")

(provide color-lexer)

(define (color-lexer in offset mode)
  (define-values (line col pos) (port-next-location in))
  (cond
    [(eof-object? (peek-char in))
     (values eof 'eof #f #f #f 0 mode)]
    [else
     (define c (read-char in))
     (define start pos)
     (cond
       ;; Whitespace
       [(char-whitespace? c)
        (values (string c) 'white-space #f start (add1 start) 0 mode)]
       
       ;; Comments (// ...)
       [(and (eqv? c #\/) (eqv? (peek-char in) #\/))
        (read-char in) ; consume second /
        (define chars (list))
        (let loop ()
            (define n (peek-char in))
            (if (or (eof-object? n) (eqv? n #\newline) (eqv? n #\return))
                (void)
                (begin
                  (set! chars (cons (read-char in) chars))
                  (loop))))
         (define str (list->string (reverse chars)))
         (values (string-append "//" str) 'comment #f start (+ start 2 (length chars)) 0 mode)]
       
       ;; String
       [(eqv? c #\")
        (define chars (list))
        (let loop ()
          (define n (peek-char in))
          (cond
            [(eof-object? n)
             (values (list->string (reverse chars)) 'error #f start (+ start 1 (length chars)) 0 mode)]
            [(eqv? n #\")
             (read-char in)
             (values "string" 'string #f start (+ start 2 (length chars)) 0 mode)]
            [else
             (read-char in)
             (set! chars (cons n chars))
             (loop)]))]

       ;; Numbers
       [(numeric? c)
        (define chars (list c))
        (let loop ()
          (define n (peek-char in))
          (cond
            [(numeric? n)
             (set! chars (cons (read-char in) chars))
             (loop)]
            [(and (eqv? n #\.) (numeric? (peek-char in 1)))
             (set! chars (cons (read-char in) chars)) ; consume .
             (let loop-fract ()
                (when (numeric? (peek-char in))
                   (set! chars (cons (read-char in) chars))
                   (loop-fract)))]))
        (values "number" 'constant #f start (+ start (length chars)) 0 mode)]

       ;; Identifiers and Keywords
       [(or (char-alphabetic? c) (eqv? c #\_))
        (define chars (list c))
        (let loop ()
          (define n (peek-char in))
          (when (and (not (eof-object? n))
                     (or (char-alphabetic? n) (char-numeric? n) (eqv? n #\_)))
            (set! chars (cons (read-char in) chars))
            (loop)))
        (define str (list->string (reverse chars)))
        (define type (if (hash-has-key? keywords str) 'keyword 'symbol))
        (values str type #f start (+ start (string-length str)) 0 mode)]

       ;; Operators and Punctuation
       [else
        (case c
          [(#\() (values "(" 'parenthesis '|(| start (add1 start) 0 mode)]
          [(#\)) (values ")" 'parenthesis '|)| start (add1 start) 0 mode)]
          [(#\{) (values "{" 'parenthesis '|{| start (add1 start) 0 mode)]
          [(#\}) (values "}" 'parenthesis '|}| start (add1 start) 0 mode)]
          [(#\, #\. #\- #\+ #\; #\/ #\*)
           (values (string c) 'symbol #f start (add1 start) 0 mode)]
          [(#\! #\= #\< #\>)
           (if (eqv? (peek-char in) #\=)
               (begin
                 (read-char in)
                 (values (string c #\=) 'symbol #f start (+ start 2) 0 mode))
               (values (string c) 'symbol #f start (add1 start) 0 mode))]
          [else
           (values (string c) 'error #f start (add1 start) 0 mode)])])]))

(define (numeric? c)
  (and (not (eof-object? c)) (char-numeric? c)))

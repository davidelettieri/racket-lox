#lang racket

(require parser-tools/lex
         racket/match
         "lexer.rkt"
         "parser.rkt"
         "pretty-print.rkt")

(define (print-token pt)
  (define t (position-token-token pt))
  (fprintf (current-output-port) "~s ~a ~a\n" (token-name t) (token-value t) "null"))

(define (parse-string s)
    (let ([in (open-input-string s)])
      (lox-parser (lambda () (lox-lexer in)))))

(define (run source)
  (syntax->datum (parse-string source)))

(define (run-file file-path)
  (define source (file->string file-path))
  (run source))

(define (run-prompt) (raise "run-prompt not implemented"))

(define args (current-command-line-arguments))

(define args-number (vector-length args))

(cond
  [(> args-number 1) (raise "Usage racket lox [script]")]
  [(= args-number 1) (run-file (vector-ref args 0))]
  [else (run-prompt)]
  )
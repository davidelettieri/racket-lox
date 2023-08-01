#lang racket

(require parser-tools/lex
         racket/match
         "lexer.rkt"
         "pretty-print.rkt")

(define (print-token pt)
  (define t (position-token-token pt))
  (fprintf (current-output-port) "~s ~a ~a\n" (token-name t) (token-value t) "null"))

(define (run source)
  (define in (open-input-string source))
  (define (get-token) (lox-lexer in))
  (define (print-tokens)
    (define t (get-token))
    (if (not (eqv? 'EOF (position-token-token t)))
      (begin
        (pretty-print t)
        (print-tokens))
      (pretty-print t)))
  (print-tokens))

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
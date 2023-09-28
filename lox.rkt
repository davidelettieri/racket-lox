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

(define (run-lox-file file)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket-lox/semantics)
    (load file)))

(define (run-prompt) (raise "run-prompt not implemented"))

(define args (current-command-line-arguments))

(define args-number (vector-length args))

(cond
  [(> args-number 1) (raise "Usage racket lox [script]")]
  [(= args-number 1) (run-lox-file (vector-ref args 0))]
  [else (run-prompt)]
  )
#lang racket

(require "lexer.rkt"
         "manual-parser.rkt")

(define stderr (open-output-file "/dev/stderr" #:exists 'append))

(define (parse-string s)
    (let ([source (open-input-string s)])
      (port-count-lines! source)
      (define in (lambda () (lox-lexer source)))
      (define tokens (get-tokens in))
      (with-handlers 
        ([exn:fail? (lambda (exn) (displayln (exn-message exn) stderr) (exit 65))])
        (parse tokens))))

(define (run source)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket-lox/semantics)
    (eval (parse-string source))))

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
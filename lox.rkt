#lang racket

(require "lexer.rkt"
         "parser.rkt")

(define stderr (open-output-file "/dev/stderr" #:exists 'append))

(define (parse-string s)
    (let ([in (open-input-string s)])
      (port-count-lines! in)
      (with-handlers 
        ([exn:fail? (lambda (exn) (displayln (exn-message exn) stderr) (exit 65))])
        (lox-parser (lambda () (lox-lexer in))))))

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
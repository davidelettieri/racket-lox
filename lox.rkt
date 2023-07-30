#lang racket

(define (run-file file) (raise "run-file not implemented"))
(define (run-prompt) (raise "run-prompt not implemented"))

(define args (current-command-line-arguments))

(define args-number (vector-length args))

(cond
    [(> args-number 1) (raise "Usage racket lox [script]")]
    [(= args-number 1) (run-file (vector-ref args 0))]
    [else (run-prompt)]
)
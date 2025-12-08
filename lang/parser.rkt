#lang racket

(require "helpers.rkt" "scanner.rkt")
(require racket/match)

(define (token->src t)
    (srcloc #f (token-line t) (token-column t) #f #f))

(define (parse tokens)
    (define _tokens (list->vector tokens))
    (define _current 0)
    (define (advance)
        (when (not (is-at-end?)) (set! _current (+ _current 1)))
        (previous))
    (define (previous) (vector-ref _tokens (- _current 1)))
    (define (peek) (vector-ref _tokens _current))
    (define (is-at-end?)
        (eqv? (token-type (peek)) 'EOF))
    (define (declaration)
        (define t (advance))
        (datum->syntax #f 
                       (token-lexeme t)
                       (token->src t)))
    (define statements '())
    (while (not (is-at-end?))
        (define decl (declaration))
        (when (not (null? decl))
            (set! statements (cons decl statements))))
    (reverse statements))

(provide parse)
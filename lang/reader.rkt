#lang racket/base

(require (only-in racket/base
            [read base-read]
            [read-syntax base-read-syntax]
            eof-object?))

(define (read in)
    (define forms
        (let loop ([acc '()])
            (define v (base-read in))
            (if (eof-object? v)
                    (reverse acc)
                    (loop (cons v acc)))))
    `(module anonymous-module racket-lox ,@forms))

(define (read-syntax src in)
    (define source (or src (object-name in)))
    (define forms
        (let loop ([acc '()])
            (define v (base-read-syntax source in))
            (if (eof-object? v)
                    (reverse acc)
                    (loop (cons v acc)))))
    (define module-stx
        `(module anonymous-module racket-lox ,@forms))
    module-stx)

(provide read read-syntax)
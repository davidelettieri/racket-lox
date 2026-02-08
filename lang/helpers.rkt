#lang racket

(require (for-syntax syntax/parse))

(define-syntax (while stx)
  (syntax-parse stx
    [(_ cond:expr body:expr ...)
     #'(let loop ()
         (when cond
           body ...
           (loop)))]))

(define-syntax (do stx)
  (syntax-parse stx
    [(_ body:expr ... (~literal while) condition:expr)
     #'(let loop ()
         body
         ...
         (when condition
           (loop)))]))

(provide while
         do)

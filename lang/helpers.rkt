#lang racket

(require (for-syntax syntax/parse))

(define-syntax (while stx)
  (syntax-parse stx
    [(_ cond:expr body:expr ...)
      #'(let loop ()
        (when cond
          body ...
          (loop)))]))

(provide while)
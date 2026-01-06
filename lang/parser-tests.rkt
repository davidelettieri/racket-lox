#lang racket

(require rackunit
         syntax/parse
         "parser.rkt"
         "scanner.rkt")

(define (parse-from-string source)
  "Helper to scan and parse source code"
  (define tokens (scan-tokens (open-input-string source)))
  (parse tokens))

(module+ test
    (test-case "parse var declaration"
        (check-eqv? (parse-from-string "var a = 1;print a;") '()))

;   ;; Tests for error detection
;   (test-case "error on missing class name"
;     (check-exn exn:fail:syntax?
;                (lambda () (parse-from-string "class { }"))))

;   (test-case "error on missing closing brace in class"
;     (check-exn exn:fail:syntax?
;                (lambda () (parse-from-string "class Foo {"))))

;   (test-case "error on missing semicolon after var declaration"
;     (check-exn exn:fail:syntax?
;                (lambda () (parse-from-string "var x = 5"))))

;   (test-case "error on missing variable name"
;     (check-exn exn:fail:syntax?
;                (lambda () (parse-from-string "var = 5;")))))
)

(provide parse-from-string)

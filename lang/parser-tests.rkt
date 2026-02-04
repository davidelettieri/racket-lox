#lang racket

(require rackunit
         syntax/parse
         "parser.rkt"
         "scanner.rkt")

(define (parse-from-string source)
  "Helper to scan and parse source code"
  (define tokens (scan-tokens (open-input-string source)))
  (parse tokens))

(define (parse-and-assert str expected)
  (check-equal?
          (map syntax->datum (parse-from-string str))
          expected))

(module+ test
    (test-case "parse var declaration with no initialization"
        (parse-and-assert "var c;" '((lox-var-declaration c lox-nil))))
    (test-case "parse var declaration with while loop"
        (parse-and-assert "var c = 0;while (c < 3) c = c + 1;"
        '((lox-var-declaration c (lox-literal 0.0))
          (lox-while (lox-binary (lox-variable c) #\< (lox-literal 3.0)) (lox-assign c (lox-binary (lox-variable c) #\+ (lox-literal 1.0))))))))

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

(provide parse-from-string)

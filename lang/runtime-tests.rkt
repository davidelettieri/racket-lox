#lang racket

(require rackunit
         racket/file)

(define (run-lox-source source)
  (define temp-file (make-temporary-file "lox-runtime-~a.rkt"))
  (define output (open-output-string))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file temp-file
                                         #:exists 'truncate/replace
                                         (lambda (port)
                                           (fprintf port "#lang racket-lox\n~a\n" source)))
                  (parameterize ([current-output-port output])
                    (dynamic-require temp-file #f))
                  (get-output-string output))
                (lambda ()
                  (when (file-exists? temp-file)
                    (delete-file temp-file)))))

(module+ test
  (test-case "print class name"
    (check-equal? (run-lox-source "class Foo {}\nprint Foo;") "Foo\n"))

  (test-case "print whole-valued numbers without trailing .0"
    (check-equal? (run-lox-source "print 1;\nprint 8 / 2;") "1\n4\n"))

  (test-case "print fractional numbers with decimals"
    (check-equal? (run-lox-source "print 1 / 2;\nprint 1.5;") "0.5\n1.5\n"))

  (test-case "print negative zero"
    (check-equal? (run-lox-source "print -0;") "-0\n"))

  (test-case "numeric equality is value-based"
    (check-equal? (run-lox-source "print 1 == 1.0;") "true\n")))

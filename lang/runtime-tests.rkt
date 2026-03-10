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
    (check-equal? (run-lox-source "class Foo {}\nprint Foo;") "Foo\n")))

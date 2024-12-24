#lang racket
 
(module reader racket
  (require syntax/strip-context "lexer.rkt" "parser.rkt")
 
  (provide (rename-out [my-read read]
                       [my-read-syntax read-syntax]))

  (define stderr (open-output-file "c:\\temp\\err" #:exists 'append))

  (define (my-read in)
    (syntax->datum
     (my-read-syntax #f in)))
 
  (define (my-read-syntax src in)
    (with-handlers
      ([exn:fail? (lambda (exn) (displayln (exn-message exn) stderr) (exit 65))])
      (with-syntax ([result (lox-parser (lambda () (lox-lexer in)))])
        (strip-context
        #'(module anything racket
            (require racket-lox/semantics)
            ; Racket prints top level expressions to output by default.
            ; We don't want that, so we wrap the parsed code in a main function
            ; returning void and evaluate main. 
            ; This way the only top level expression is the main function result and it is void which 
            ; doesn't get printed.
            (define (main)
              result
              (void))
              (main)))))))
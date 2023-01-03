#lang racket/base

(require rackunit
         "scanner.rkt")

(check-equal? (get-tokens "()") (list (token 'LEFT_PAREN 1 0 1 #f) (token 'RIGHT_PAREN 1 1 2 #f) (token 'EOF 1 2 3 #f)))
(check-equal? (get-tokens "//") (list (token 'EOF 1 2 3 #f)))
(check-equal? (get-tokens "\"test\"") (list (token 'STRING 1 0 1 "test") (token 'EOF 1 6 7 #f)))
(check-equal? (get-tokens "123") (list (token 'NUMBER 1 0 1 123) (token 'EOF 1 3 4 #f)))
(check-equal? (get-tokens "dav") (list (token 'IDENTIFIER 1 0 1 "dav") (token 'EOF 1 3 4 #f)))
(check-equal? (get-tokens "dav1") (list (token 'IDENTIFIER 1 0 1 "dav1") (token 'EOF 1 4 5 #f)))
(check-equal? (get-tokens "for") (list (token 'FOR 1 0 1 #f) (token 'EOF 1 3 4 #f)))
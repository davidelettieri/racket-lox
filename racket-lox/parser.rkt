#lang racket

(require "stmt.rkt"
         "expr.rkt")

(define-syntax-rule (while test body ...)
  (local [(define (loop)
            (when test
              body ...
              (loop)))]
    (loop)))

(define-syntax-rule (do-while test body ...)
(begin
  body ...
  (local [(define (loop)
            (when test
              body ...
              (loop)))]
    (loop)))) 

(require "scanner.rkt")

(define current 0)
(define tokens #f)
(struct parse-error exn:fail:user ())


(provide parse)

(define (parse src in)
  (set! tokens (list->vector get-tokens in))
  (define statements (list))
  (while (not (is-at-end?)) (set! statements (cons (declaration) statements)))
  statements)

(define declaration
  (with-handlers ([parse-error?
                   (begin (syncronize) #f)])
  (cond
    ((match? 'CLASS) (class-declaration))
    ((and (check? 'FUN) (check-next? 'IDENTIFIER))
     (consume 'FUN #f)
     (function "function"))
    ((match? 'VAR) (var-declaration))
    (else (statement)))))

(define (class-declaration)
  (define name (consume 'IDENTIFIER "Expected class name"))
  (define superclass #f)
  (when (match? 'LESS)
    ((consume 'IDENTIFIER "Expected superclass name")
     (set! superclass (expr-variable (previous)))))
  (consume 'LEFT_BRACE "Expect '{' before class body")
  (define methods (list))
  (while (and (not (check? 'RIGHT_BRACE)) (not (is-at-end?)))
         (append methods (list (function "method"))))
  (consume 'RIGHT_BRACE "Expect '}' after class body")
  (stmt-class name superclass methods))

(define (function kind)
  (define name (consume 'IDENTIFIER (format "Expect ~a name." kind)))
  (consume 'LEFT_PAREN (format "Expect '(' after ~a name." kind))
  (define parameters '())
  (unless (check? 'RIGHT_PAREN)
    (do-while (match? 'COMMA)
              (append (consume 'IDENTIFIER "Expect parameter name.") parameters)))
  (consume 'RIGHT_PAREN "Expect ')' after parameters")
  (consume 'LEFT_BRACE "Expect '{' after parameters")
  (define body (block))
  (stmt-function name parameters body))

(define (var-declaration)
  (define name (consume 'IDENTIFIER "Expect variable name."))
  (define initializer #f)
  (when (match? 'EQUAL)
    (set! initializer (expression)))
  (consume 'SEMI_COLON "Expect ';' after variable declaration")
  (stmt-var name initializer))

(define (statement)
  (cond
    ((match? 'FOR) ( for-statement))
    ((match? 'IF) (if-statement))
    ((match? 'PRINT) (print-statement))
    ((match? 'RETURN) (return-statement))
    ((match? 'WHILE) (while-statement))
    ((match? 'LEFT_BRACE) (stmt-block (block)))
    (else (expression-statement))))

(define (block)
  (define statements '())
  (while (and (not (check? 'RIGHT_BRACE)) (not (is-at-end?)))
         (append (declaration) statements))
  (consume 'RIGHT_BRACE "Expect '}' after block.")
  (statements))

(define (expression) (assignment))

(define is-at-end? (eof-object? (peek)))

(define syncronize
  ((advance)
  (unless
      (or
       (not (is-at-end?))
       (eqv? (token-type (previous)) 'SEMICOLON)
       (memv (token-type (peek)) (list 'CLASS 'FUN 'VAR 'FOR 'IF 'WHILE 'PRINT 'RETURN)))
    syncronize)))

(define (match? types)
  ((define found #f)
   (for ([type types])
     #:break found
     (when (check? type)
         ((set! found #t)
          (advance))))
   found))

(define (check? type)
  (if (is-at-end?)
      #f
      (eqv? type (token-type (peek)))))

(define (check-next? type)
  (cond
    ((is-at-end?) #f)
    ((eof-object? (vector-ref tokens (+ current 1))) #f)
    (else (eqv? (token-type (vector-ref tokens (+ current 1))) type))))

(define (previous) (vector-ref tokens (- current 1)))

(define (consume type message)
  (if (check? type)
      (advance)
      (error (peak) message)))
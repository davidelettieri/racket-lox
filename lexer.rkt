#lang racket

(require parser-tools/lex
         parser-tools/lex-sre)

(define-tokens basic-tokens (NUMBER STRING IDENTIFIER))
(define-empty-tokens punct-tokens (LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE
                                   COMMA DOT MINUS PLUS SEMICOLON SLASH STAR

                                  ; One or two character tokens.
                                  BANG BANG_EQUAL
                                  EQUAL EQUAL_EQUAL
                                  GREATER GREATER_EQUAL
                                  LESS LESS_EQUAL

                                  ; Keywords.
                                  AND CLASS ELSE FALSE FUN FOR IF NIL OR
                                  PRINT RETURN SUPER THIS TRUE VAR WHILE

                                  EOF))

(define-lex-abbrev lox-number 
  (union 
    (concatenation (? #\-) (repetition 1 +inf.0 numeric))
    (concatenation #\. (repetition 1 +inf.0 numeric))))

(define lox-lexer
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(concatenation "//" (repetition 0 +inf.0 (char-complement #\newline))) (return-without-pos (lox-lexer input-port))]
   ["(" (token-LEFT_PAREN)]
   [")" (token-RIGHT_PAREN)]
   ["{" (token-LEFT_BRACE)]
   ["}" (token-RIGHT_BRACE)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   ["-" (token-MINUS)]
   ["+" (token-PLUS)]
   [";" (token-SEMICOLON)]
   ["/" (token-SLASH)]
   ["*" (token-STAR)]
   ["!" (token-BANG)]
   ["!=" (token-BANG_EQUAL)]
   ["=" (token-EQUAL)]
   ["==" (token-EQUAL_EQUAL)]
   [">" (token-GREATER)]
   [">=" (token-GREATER_EQUAL)]
   ["<" (token-LESS)]
   ["<=" (token-LESS_EQUAL)]
   ["and" (token-AND)]
   ["class" (token-CLASS)]
   ["else" (token-ELSE)]
   ["false" (token-FALSE)]
   ["fun" (token-FUN)]
   ["for" (token-FOR)]
   ["if" (token-IF)]
   ["nil" (token-NIL)]
   ["or" (token-OR)]
   ["print" (token-PRINT)]
   ["return" (token-RETURN)]
   ["super" (token-SUPER)]
   ["this" (token-THIS)]
   ["true" (token-TRUE)]
   ["while" (token-WHILE)]
   ["var" (token-VAR)]
   [lox-number  (token-NUMBER (string->number lexeme))]
   ; invoke the lexer again to skip the current token
   ; the return-without-pos call is needed to avoid a "double" wrapping into a position token
   ; ref. https://github.com/racket/parser-tools/blob/b08f6137a3c067720c4b4723dd726652af288e97/parser-tools-lib/parser-tools/yacc.rkt#L247
   [whitespace (return-without-pos (lox-lexer input-port))]
   [#\newline (return-without-pos (lox-lexer input-port))]
   [(concatenation (union alphabetic (char-set "_")) (repetition 0 +inf.0 (union alphabetic numeric (char-set "_")))) (token-IDENTIFIER lexeme)]))

(provide lox-lexer basic-tokens punct-tokens)
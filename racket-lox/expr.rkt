#lang racket

(struct expr-binary (left operator right))
(struct expr-call (callee paren arguments))
(struct expr-get (obj name))
(struct expr-grouping (expr))
(struct expr-literal (value))
(struct expr-logical (left operator right))
(struct expr-unary (operator right))
(struct expr-set (obj name value))
(struct expr-super (keyword method))
(struct expr-this (keyword))
(struct expr-variable (name))
(struct expr-assign (name value))
(struct expr-anon-function (parameters body))

(provide expr-binary
         expr-call
         expr-get
         expr-grouping
         expr-literal
         expr-logical
         expr-unary
         expr-set
         expr-super
         expr-this
         expr-variable
         expr-assign
         expr-anon-function)
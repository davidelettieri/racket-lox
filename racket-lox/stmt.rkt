#lang racket

(struct stmt-block (statements))
(struct stmt-class (name superclass methods))
(struct stmt-expression (expr))
(struct stmt-function (name parameters body)) 
(struct stmt-if (condition thenBranch elseBranch))
(struct stmt-print (expr))
(struct stmt-return (keyword value))
(struct stmt-var (name initializer))
(struct stmt-while (condition body))

(provide stmt-block
         stmt-class
         stmt-expression
         stmt-function
         stmt-if
         stmt-print
         stmt-return
         stmt-var
         stmt-while)
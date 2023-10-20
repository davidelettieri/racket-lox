#lang racket-lox

print -(3); // expect: -3
print --(3); // expect: 3
print ---(3); // expect: -3


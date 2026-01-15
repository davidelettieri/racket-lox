#lang racket-lox

var a = 0;
{
  var a = 10;
  a = a + 1;
  print a;
}

print a;
#lang info

(define collection 'multi)

(define deps
  '())
(define build-deps
  '("aws-cloudformation-template-lib"
    "base"
    "racket-doc"
    ["scribble-lib" #:version "1.16"]
    "threading-lib"
    "turnstile"))

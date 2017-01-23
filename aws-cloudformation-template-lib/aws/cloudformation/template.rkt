#lang racket/base

(module reader syntax/module-reader aws/cloudformation/template)

(require aws/cloudformation/template/base
         aws/cloudformation/template/resources)
(provide (except-out (all-from-out aws/cloudformation/template/base)
                     hash-percent-module-begin hash-percent-app hash-percent-datum)
         (all-from-out aws/cloudformation/template/resources)
         (rename-out [hash-percent-module-begin #%module-begin]
                     [hash-percent-app #%app]
                     [hash-percent-datum #%datum]))

#lang scribble/manual

@(require (for-label racket/base
                     (except-in aws/cloudformation/template #%module-begin #%app #%datum))
          (for-syntax racket/base
                      racket/list
                      syntax/parse/experimental/template
                      threading)
          (prefix-in cf: aws/cloudformation/template/base)
          (only-in macrotypes/typecheck typeof)
          scribble/core
          scribble/example
          syntax/parse/define)

@(define (make-cf-eval)
   ((make-eval-factory '() #:lang 'aws/cloudformation/template)))

@(define-simple-macro (cf-interactions body ...)
   (examples #:eval (make-cf-eval)
             #:once
             #:label #f
             #:preserve-source-locations
             body ...))

@(define (yellow . content)
   (make-element (make-style #f (list (make-background-color-property "yellow"))) content))

@title{AWS CloudFormation Template DSL}

@author{@author+email["Alexis King" "lexi.lambda@gmail.com"]}

@defmodule[aws/cloudformation/template #:lang]

@nested[#:style 'inset]{
 @yellow{@bold{NOTE}}: This library is @emph{unstable}; compatibility will not be maintained.}

Amazon Web Services provides a service called @hyperlink["https://aws.amazon.com/cloudformation/"]{
CloudFormation}, a tool for managing the creation, update, and removal of entire @emph{stacks} of
services at once, which is useful for complex deployments that use many different AWS services for a
single application. These stacks are defined using declarative @emph{templates}, which are then
interpreted by CloudFormation.

For certain scenarios, CloudFormation is an invaluable tool, but templates can quickly become unwieldy
and difficult to maintain. They can be written in JSON or YAML, but support features that simple
configuration formats are ill-equipped to handle, such as including variable references and even calls
to predefined functions. Additionally, the resource types can be quite complex, and it can be easy to
forget what resources expect or allow certain options.

This library provides a statically typed DSL for constructing CloudFormation templates. Running a
module written in the @racketmodname[aws/cloudformation/template] language will produce a JSON
template on stdout that can be directly provided to AWS.

@table-of-contents[]

@section{Example}

Here’s a simple demonstration of a template that uses some of the features that this library provides:

@(racketmod
  #:file "template.rkt"
  aws/cloudformation/template

  (defparam network-service-subnets : (List String)
    #:description "The subnets to which the load balancer attaches.")

  (defparam cluster : Resource
    #:description "The ECS cluster for container deployment")

  (defparam task-definition : Resource
    #:description "The ECS task definition containing the container to run")

  (defresource load-balancer
    (aws:elastic-load-balancing:load-balancer
     #:listeners [{ #:instance-port 8080
                    #:instance-protocol "HTTP"
                    #:load-balancer-port 80
                    #:protocol "HTTP" }]
     #:subnets network-service-subnets))

  (defresource service
    (aws:ecs:service
     #:cluster cluster
     #:role "ecsServiceRole"
     #:load-balancers [{ #:container-name "http-server"
                         #:container-port 8080
                         #:load-balancer-name load-balancer }]
     #:desired-count 2
     #:task-definition task-definition)))

Notably, incorrectly using a resource type will produce a type error:

@(cf-interactions
  (eval:error (aws:elastic-load-balancing:load-balancer))
  (eval:error (aws:elastic-load-balancing:load-balancer #:listeners #f))
  (eval:error (aws:elastic-load-balancing:load-balancer
               #:listeners [{}]))
  (eval:error (aws:elastic-load-balancing:load-balancer
               #:listeners [{ #:instance-port "8080"
                              #:load-balancer-port "80"
                              #:protocol "HTTP" }]))
  (aws:elastic-load-balancing:load-balancer
   #:listeners [{ #:instance-port 8080
                  #:load-balancer-port 80
                  #:protocol "HTTP" }]))

@section{Reference}

@subsection{Base Types}

@deftogether[(@defidform[#:kind "type" String]
              @defidform[#:kind "type" Number]
              @defidform[#:kind "type" Boolean])]{
The types of raw data. String literals are surrounded with @litchar{"} double quotes @litchar{"},
numbers are integers or decimals, and booleans are written as @racket[#t] or @racket[#f].}

@deftogether[(@defform[#:kind "type" (List type)]
              @defform[#:link-target? #f #:id list [elem-expr ...]])]{
The type of lists and list literals, which correspond to arrays in AWS CloudFormation templates. A
list literal can be written by surrounding elements with @litchar{[} square brackets @litchar{]}. All
elements of a list must be the same type.

@(cf-interactions
  ["hello" "world"]
  [{ #:x 0 #:y 0 } { #:x 10 #:y 5 }])}

@deftogether[(@defform[#:kind "type" (Dict type)]
              @defform[(dict kw elem-expr ... ...)])]{
The type of dictionaries and dictionary literals, which correspond to arbitrary JSON objects in AWS
CloudFormation templates.

@(cf-interactions
  (dict #:x "hello"
        #:y "world"))}

@deftogether[(@defform[#:kind "type" (Record record-spec ...)
                       #:grammar ([record-spec required-spec
                                   optional-spec]
                                  [required-spec (code:line #:<key> val-type)]
                                  [optional-spec [#:<key> val-type]])]
              @defform[#:link-target? #f #:id record { #:<key> val-expr ... ... @#,elem{ }}])]{
The type of records, which are heterogenous sets of key/value pairs. All keys must be keywords, but
values can be of any type. Key/type pairs wrapped with @litchar{[} square brackets @litchar{]} are
optional.

Record literals are written as keyword/expression pairs surrounded by @litchar["{"] curly braces
@litchar["}"].

@(cf-interactions
  {}
  { #:x 0 #:y 0 }
  { #:foo { #:bar { #:baz #f } } })}

@defidform[#:kind "type" Resource]{
The type of all AWS resources.}

@subsection{Definition Forms}

@defform[#:literals [:]
         (defparam id : type
           #:description description-str)]{
Declares a parameter named @racket[id] as an input to a template with the type @racket[type] and
description @racket[description-str], which must be a string literal. The parameter can be referenced
elsewhere in the template.}

@defform[(defresource id resource-expr)
         #:contracts ([resource-expr Resource])]{
Declares a resource to be created by this template with the name @racket[id] and described by
@racket[resource-expr]. The resource can be referenced elsewhere in the template.}

@subsection{Resources}

@(begin-for-syntax
   (define (reset-srcloc stx [line 1] [col 0])
     (datum->syntax stx
                    (syntax-e stx)
                    (list (syntax-source stx)
                          line
                          col
                          (syntax-position stx)
                          (syntax-span stx))
                    stx))

   (define (adjust-srclocs stx line col)
     (define new-loc
       (list (syntax-source stx)
             (+ (syntax-line stx) line)
             (+ (syntax-column stx) col)
             (syntax-position stx)
             (syntax-span stx)))
     (syntax-parse stx
       [(a . b)
        (datum->syntax stx
                       (cons (adjust-srclocs #'a line col)
                             (adjust-srclocs #'b line col))
                       new-loc
                       stx)]
       [other (datum->syntax stx (syntax-e #'other) new-loc stx)]))

   (define syntax-max-line
     (syntax-parser
       [(a . b)
        (apply max (filter values (list (syntax-line this-syntax)
                                        (syntax-max-line #'a)
                                        (syntax-max-line #'b))))]
       [_ (syntax-line this-syntax)]))

   (define syntax-min-line
     (syntax-parser
       [(a . b)
        (apply min (filter values (list (syntax-line this-syntax)
                                        (syntax-min-line #'a)
                                        (syntax-min-line #'b))))]
       [_ (syntax-line this-syntax)]))

   (define (syntax-line-span stx)
     (if (or (syntax-max-line stx)
             (syntax-min-line stx))
         (add1 (- (syntax-max-line stx)
                  (syntax-min-line stx)))
         0))

   (define (stx-kw-len stx)
     (+ 2 (string-length (keyword->string (syntax-e stx)))))

   (define-syntax-rule (first-value expr)
     (call-with-values (λ () expr) (λ (x . xs) x)))

   (define-syntax-class cf-type
     ; do a ton of srcloc munging in here in order to produce s-expressions that @racket[] can arrange
     ; in a pretty/readable way
     #:attributes [elem]
     [pattern cf:~Number #:attr elem (reset-srcloc #'Number)]
     [pattern cf:~Boolean #:attr elem (reset-srcloc #'Boolean)]
     [pattern cf:~String #:attr elem (reset-srcloc #'String)]
     [pattern cf:~Resource #:attr elem (reset-srcloc #'Resource)]
     [pattern (cf:~List t:cf-type)
      #:attr elem (reset-srcloc #`(#,(reset-srcloc #'List 1 1) #,(adjust-srclocs #'t.elem 0 6)))]
     [pattern (cf:~Dict t:cf-type)
      #:attr elem (reset-srcloc #`(#,(reset-srcloc #'Dict 1 1) #,(adjust-srclocs #'t.elem 0 6)))]
     [pattern (cf:~Record [(kw_req t_req:cf-type) ...] [(kw_opt t_opt:cf-type) ...])
      #:with [(kw_req* t_req*) ...]
             (reverse
              (first-value
               (for/fold ([lst '()]
                          [n 0])
                         ([kw (in-list (attribute kw_req))]
                          [t (in-list (attribute t_req.elem))])
                 (let ([stx (reset-srcloc #`(#,(reset-srcloc kw (add1 n) 8)
                                             #,(adjust-srclocs t n (+ 9 (stx-kw-len kw))))
                                          (add1 n) 8)])
                   (values (cons stx lst)
                           (+ n (syntax-line-span stx)))))))
      #:with [arg_opt* ...]
             (reverse
              (first-value
               (for/fold ([lst '()]
                          [n (syntax-line-span (syntax->list #'[kw_req* ... t_req* ...]))])
                         ([kw (in-list (attribute kw_opt))]
                          [t (in-list (attribute t_opt.elem))])
                 (let ([stx (reset-srcloc #`[#,(reset-srcloc kw (add1 n) 9)
                                             #,(adjust-srclocs t n (+ 10 (stx-kw-len kw)))]
                                          (add1 n) 8)])
                   (values (cons stx lst)
                           (+ n (syntax-line-span stx)))))))
      #:attr elem (reset-srcloc (quasitemplate (#,(reset-srcloc #'Record 1 1)
                                                {?@ kw_req* t_req*} ...
                                                arg_opt* ...)))]))

@(define-syntax-parser define-resource-type
   [(_ sym:id)
    #:with id (datum->syntax #'here (syntax-e #'sym))
    #:with expr #'(let ()
                    (local-require (only-in aws/cloudformation/template/resources id))
                    id)
    #:with ({~literal let-values} () ({~literal let-values} () expanded))
           (local-expand #'expr 'expression '())
    #:with (cf:~-> []
                   [(kw_req arg_req:cf-type) ...]
                   [(kw_opt arg_opt:cf-type) ...]
                   _)
           (typeof #'expanded)
    #:with [id_req ...] (map (λ~> syntax-e keyword->string string->symbol) (attribute kw_req))
    #:with [id_opt ...] (map (λ~> syntax-e keyword->string string->symbol) (attribute kw_opt))
    #'(defproc (id (kw_req id_req arg_req.elem) ...
                   (kw_opt id_opt arg_opt.elem @#,italic{AWS default}) ...)
        Resource)])

@(define-syntax-parser define-all-resource-types!
   [(_)
    #:do [(define-values [stxs vals]
            (module->exports 'aws/cloudformation/template/resources))]
    #:with [sym ...] (map first (cdr (assoc 0 vals)))
    #'(begin (define-resource-type sym) ...)])

@(define-all-resource-types!)

@section{AWS Intrinsic Functions and Pseudo Parameters}

@defthing[aws:region String]{
Returns a string representing the AWS Region in which the encompassing resource is being created, such
as @tt{us-west-2}.}

@defthing[aws:stack-name String]{
Returns the name of the stack.}

@defproc[(fn:or [x Boolean] [y Boolean]) Boolean]{
Returns @racket[#t] if either @racket[x] or @racket[y] evaluate to @racket[#t], otherwise returns
@racket[#f].}

@defproc[(fn:join [#:separator separator String ""] [strs (List String)]) String]{
Concatenates a set of strings into a single string, separated by @racket[separator].}

@defproc[(fn:if [condition Boolean] [x a] [y a]) a]{
Returns @racket[x] if @racket[condition] evaluates to @racket[#t], otherwise returns @racket[y].}

@defproc[(fn:equal? [x a] [y a]) Boolean]{
Returns @racket[#t] if @racket[x] and @racket[y] are equivalent values, otherwise returns
@racket[#f].}

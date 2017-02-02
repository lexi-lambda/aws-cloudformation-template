#lang curly-fn turnstile

(module reader syntax/module-reader aws/cloudformation/template/base)

(require (for-syntax syntax/parse/class/local-value
                     syntax/parse/class/paren-shape
                     syntax/parse/experimental/template
                     threading)
         json
         racket/splicing
         racket/stxparam)

(provide (for-syntax lisp-case->upper-camel-case
                     ~Boolean ~Number ~String ~List ~-> ~Record ~Dict ~Resource)
         #%top #%top-interaction hash-percent-module-begin hash-percent-app hash-percent-datum
         Boolean Number String List -> Record Dict Resource
         ann dict def defparam defresource fn:if fn:equal? fn:get-att
         (typed-out [aws:region : String]
                    [aws:stack-name : String]
                    [fn:or : (-> Boolean Boolean Boolean)]
                    [fn:join : (-> (List String) [#:separator String] String)]))

(define-syntax-parameter params
  (λ (stx) (raise-syntax-error #f "cannot use outside of cloudformation template" stx)))
(define-syntax-parameter resources
  (λ (stx) (raise-syntax-error #f "cannot use outside of cloudformation template" stx)))

(define-base-type Boolean)
(define-base-type Number)
(define-base-type String)
(define-base-type Resource)

(define-type-constructor List #:arity = 1)
(define-type-constructor Dict #:arity = 1)

(begin-for-syntax
  (define (sort-kw-pairs kws xs)
    (sort (map cons kws xs) keyword<? #:key (compose1 syntax-e car)))

  (define lisp-case->upper-camel-case
    (λ~> (regexp-replace #px"^[a-z]" _ string-upcase)
         (regexp-replace* #px"-([a-z])" _ #{string-upcase %2}))))

(define (->-internal) (void-))
(define-syntax-parser ->
  [(_ {~or {~seq kw:keyword τ_kw:type} [kw_opt:keyword τ_opt:type] τ_pos:type} ... τ_result:type)
   #:fail-when (check-duplicates (syntax->list #'[kw ... kw_opt ...]) eq? #:key syntax-e)
               "duplicate keyword argument"
   #:with [(kw* . τ_kw*) ...] (sort-kw-pairs (attribute kw) (attribute τ_kw.norm))
   #:with [(kw_opt* . τ_opt*) ...] (sort-kw-pairs (attribute kw_opt) (attribute τ_opt.norm))
   (⊢ (->-internal [list- τ_pos.norm ...]
                   [list- (cons- 'kw* τ_kw*) ...]
                   [list- (cons- 'kw_opt* τ_opt*) ...]
                   τ_result.norm)
      : #%type)])
(begin-for-syntax
  (define-syntax ~->
    (pattern-expander
     (syntax-parser
       [(_ . pat)
        #'(... {~and ({~literal #%plain-app}
                      {~literal ->-internal}
                      ({~literal #%plain-app} {~literal list-} τ_pos ...)
                      ({~literal #%plain-app}
                       {~literal list-}
                       ({~literal #%plain-app} {~literal cons-} ({~literal quote} kw) τ_kw) ...)
                      ({~literal #%plain-app}
                       {~literal list-}
                       ({~literal #%plain-app} {~literal cons-} ({~literal quote} kw_opt) τ_opt) ...)
                      τ_result)
                     {~parse pat #'([τ_pos ...] [(kw τ_kw) ...] [(kw_opt τ_opt) ...] τ_result)}})]))))

(define (Record-internal) (void-))
(define-syntax-parser Record
  [(_ {~or {~seq kw:keyword τ:type} [kw_opt:keyword τ_opt:type]} ...)
   #:fail-when (check-duplicates (syntax->list #'[kw ... kw_opt ...]) eq? #:key syntax-e)
               "duplicate record key"
   #:with [(kw* . τ*) ...] (sort-kw-pairs (attribute kw) (attribute τ.norm))
   #:with [(kw_opt* . τ_opt*) ...] (sort-kw-pairs (attribute kw_opt) (attribute τ_opt.norm))
   (⊢ (Record-internal (list- (cons- 'kw* τ*) ...)
                       (list- (cons- 'kw_opt* τ_opt*) ...))
      : #%type)])
(begin-for-syntax
  (define-syntax ~Record
    (pattern-expander
     (syntax-parser
       [(_ . pat)
        #'(... {~and ({~literal #%plain-app}
                      {~literal Record-internal}
                      ({~literal #%plain-app} {~literal list-}
                       ({~literal #%plain-app} {~literal cons-} ({~literal quote} kw) τ) ...)
                      ({~literal #%plain-app} {~literal list-}
                       ({~literal #%plain-app} {~literal cons-} ({~literal quote} kw_opt) τ_opt) ...))
                     {~parse pat #'([(kw τ) ...] [(kw_opt τ_opt) ...])}})])))
  (let ([old-type=? (current-type=?)])
    (current-type=?
     (λ (a b)
       (syntax-parse (list a b)
         [((~Record [(kw_req_a τ_req_a) ...] [(kw_opt_a τ_opt_a) ...])
           (~Record [(kw_req_b τ_req_b) ...] [(kw_opt_b τ_opt_b) ...]))
          #:with [[kw_a ...] [τ_a ...]] #'[[kw_req_a ... kw_opt_a ...] [τ_req_a ... τ_opt_a ...]]
          #:with [[kw_b ...] [τ_b ...]] #'[[kw_req_b ... kw_opt_b ...] [τ_req_b ... τ_opt_b ...]]
          (and (= (length (attribute kw_req_a)) (length (attribute kw_req_b)))
               (= (length (attribute kw_opt_a)) (length (attribute kw_opt_b)))
               (andmap string=?
                       (map (compose1 keyword->string syntax-e) (attribute kw_a))
                       (map (compose1 keyword->string syntax-e) (attribute kw_b)))
               (andmap (current-type=?) (attribute τ_a) (attribute τ_b)))]
         [_ (old-type=? a b)])))))

(begin-for-syntax
  (let ([old-typecheck-relation (current-typecheck-relation)])
    (current-typecheck-relation
     (λ (a b)
       (syntax-parse (list a b)
         ;; record subtyping
         [((~Record [(kw_req_a τ_req_a) ...] [(kw_opt_a τ_opt_a) ...])
           (~Record [(kw_req_b τ_req_b) ...] [(kw_opt_b τ_opt_b) ...]))
          (or ((current-type=?) a b)
              ; to keep things simpler, don’t allow record subtyping when the given record has
              ; optional fields in the type
              (and (zero? (length (attribute kw_opt_a)))
                   ; ensure all the required keys are present
                   (let ([kws_provided (map syntax-e (attribute kw_req_a))])
                     (andmap #{member % kws_provided} (map syntax-e (attribute kw_req_b))))
                   ; create a dictionary of all the allowed mappings for the expected type
                   (let ([kw/τ-allowed (make-immutable-hash
                                        (append (map cons
                                                     (map syntax-e (attribute kw_req_b))
                                                     (attribute τ_req_b))
                                                (map cons
                                                     (map syntax-e (attribute kw_opt_b))
                                                     (attribute τ_opt_b))))])
                     ; check the types match for all provided mappings
                     (for/and ([kw (in-list (map syntax-e (attribute kw_req_a)))]
                               [τ (in-list (attribute τ_req_a))])
                       (let ([τ_expected (hash-ref kw/τ-allowed kw #f)])
                         (and τ_expected
                              ((current-typecheck-relation) τ τ_expected)))))))]
         [_ (old-typecheck-relation a b)])))))

(begin-for-syntax
  (define (type->aws-param-type τ-stx)
    (define/syntax-parse τ:type τ-stx)
    (syntax-parse #'τ.norm
      #:context 'type->aws-param-type
      [~String "String"]
      [~Number "Number"]
      [~Resource "String"]
      [(~List τ:type) (string-append "List<" (type->aws-param-type #'τ) ">")]
      [other (type-error #:src τ-stx
                         #:msg "~a is not usable as an AWS parameter type" τ-stx)])))

(define-syntax-parser hash-percent-app
  ;; function application
  [(~parens ~! _ rest ...+)
   ((syntax-local-value #'fn-app)
    (syntax/loc this-syntax
      (rest ...)))]
  ;; list literals
  [[~brackets ~! _ rest ...]
   (syntax/loc this-syntax
     (list rest ...))]
  ;; record literals
  [{~braces ~! _ rest ...}
   (syntax/loc this-syntax
     (record rest ...))])

(define-typed-syntax fn-app
  [(e_fn {~or {~seq kw_e:keyword e_kw:expr} e_pos:expr} ...) ≫
   [⊢ e_fn ≫ e_fn- ⇒ (~-> [τ_pos ...] [(kw_τ τ_kw) ...] [(kw_opt τ_opt) ...] τ_result)]
   #:fail-unless (stx-length=? #'[τ_pos ...] #'[e_pos ...])
                 (format "arity mismatch; expected ~a positional args, given ~a"
                         (stx-length #'[τ_pos ...])
                         (stx-length #'[e_pos ...]))
   #:with [(kw_e* . e_kw*) ...] (sort-kw-pairs (attribute kw_e) (attribute e_kw))
   #:do [(define kw_e-datum (syntax->datum #'[kw_e* ...]))
         (define kw_required-datum (syntax->datum #'[kw_τ ...]))
         (define kw_allowed-datum (syntax->datum #'[kw_τ ... kw_opt ...]))
         (define missing-kws (filter-not #{member % kw_e-datum} kw_required-datum))
         (define invalid-kws (filter-not #{member % kw_allowed-datum} kw_e-datum))]
   #:fail-when (not (empty? missing-kws))
               (format "arity mismatch; expected ~a keyword argument(s), but they were not supplied"
                       (string-join (map ~a missing-kws) ", "))
   #:fail-when (not (empty? invalid-kws))
               (format "arity mismatch; unexpected ~a keyword argument(s)"
                       (string-join (map ~a invalid-kws) ", "))
   #:do [(define kw/τ-dict (make-immutable-hasheq
                            (map cons kw_allowed-datum (syntax->list #'[τ_kw ... τ_opt ...]))))]
   #:with [(kw_supplied e_supplied τ_supplied) ...]
          (for/list ([kw (in-list kw_e-datum)]
                     [e (in-list (attribute e_kw*))])
            (list kw e (hash-ref kw/τ-dict kw)))
   [⊢ e_pos ≫ e_pos- ⇐ τ_pos] ...
   [⊢ e_supplied ≫ e_supplied- ⇐ τ_supplied] ...
   -------------------------
   [⊢ #,(template (#%app- e_fn- e_pos- ... (?@ kw_supplied e_supplied-) ...)) ⇒ τ_result]])

(define-typed-syntax list
  [(_ e_elem ...) ⇐ (~List τ_elem) ≫
   #:with ~! #f
   [⊢ e_elem ≫ e_elem- ⇐ τ_elem] ...
   -------------------------
   [⊢ (list- e_elem- ...)]]
  [(_ e_elem ...+) ≫
   [⊢ e_elem ≫ e_elem- ⇒ τ_elem] ...
   #:with ~! #f
   #:fail-unless (same-types? #'[τ_elem ...])
                 "all elements of a list must be the same type"
   #:with τ_elem* (first (attribute τ_elem))
   -------------------------
   [⊢ (list- e_elem- ...) ⇒ (List τ_elem*)]])

(define-typed-syntax record
  [(_ {~seq kw:keyword e:expr} ...) ⇐ (~Record [(kw_req τ_req) ...] [(kw_opt τ_opt) ...]) ≫
   #:with ~! #f
   #:with [(kw* . e*) ...] (sort-kw-pairs (attribute kw) (attribute e))
   #:do [(define kw-datum (syntax->datum #'[kw* ...]))
         (define kw_required-datum (syntax->datum #'[kw_req ...]))
         (define kw_allowed-datum (syntax->datum #'[kw_req ... kw_opt ...]))
         (define missing-kws (filter-not #{member % kw-datum} kw_required-datum))
         (define invalid-kws (filter-not #{member % kw_allowed-datum} kw-datum))]
   #:fail-when (not (empty? missing-kws))
               (format "expected ~a key(s), but they were not supplied"
                       (string-join (map ~a missing-kws) ", "))
   #:fail-when (not (empty? invalid-kws))
               (format "unexpected ~a key(s)"
                       (string-join (map ~a invalid-kws) ", "))
   #:do [(define kw/τ-dict (make-immutable-hasheq
                            (map cons kw_allowed-datum (syntax->list #'[τ_req ... τ_opt ...]))))]
   #:with [(kw_supplied e_supplied τ_supplied) ...]
          (for/list ([kw (in-list kw-datum)]
                     [e (in-list (attribute e*))])
            (list kw e (hash-ref kw/τ-dict kw)))
   [⊢ e_supplied ≫ e_supplied- ⇐ τ_supplied] ...
   #:with [sym ...] (map (λ~> syntax-e keyword->string
                              lisp-case->upper-camel-case string->symbol)
                         (attribute kw))
   -------------------------
   [⊢ #,(template (hash- {?@ 'sym e_supplied-} ...))]]
  [(_ {~seq kw:keyword e:expr} ...) ≫
   [⊢ e ≫ e- ⇒ τ] ...
   #:with [sym ...] (map (λ~> syntax-e keyword->string
                              lisp-case->upper-camel-case string->symbol)
                         (attribute kw))
   -------------------------
   [⊢ #,(template (hash- (?@ 'sym e-) ...))
      ⇒ #,(template (Record (?@ kw τ) ...))]])

(define-typed-syntax dict
  [(_ {~seq kw:keyword e:expr} ...) ⇐ (~Dict τ_elem) ≫
   #:with ~! #f
   #:fail-when (check-duplicates (attribute kw) eq? #:key syntax-e)
               "duplicate dictionary key"
   [⊢ e ≫ e- ⇐ τ_elem] ...
   #:with [key-sym ...] (map (compose1 string->symbol keyword->string syntax-e) (attribute kw))
   -------------------------
   [⊢ #,(template (hash- {?@ 'key-sym e-} ...))]]
  [(_ {~seq kw:keyword e:expr} ...) ≫
   #:with ~! #f
   #:fail-when (check-duplicates (attribute kw) eq? #:key syntax-e)
               "duplicate dictionary key"
   [⊢ e ≫ e- ⇒ τ_elem] ...
   #:fail-unless (same-types? #'[τ_elem ...])
                 "all elements of a dictionary must be the same type"
   #:with τ_elem* (first (attribute τ_elem))
   #:with [key-sym ...] (map (compose1 string->symbol keyword->string syntax-e) (attribute kw))
   -------------------------
   [⊢ #,(template (hash- {?@ 'key-sym e-} ...)) ⇒ (Dict τ_elem*)]])

(define-typed-syntax hash-percent-datum
  [(_ . n:number) ≫
   -------------------------
   [⊢ (#%datum- . n) ⇒ Number]]
  [(_ . s:str) ≫
   -------------------------
   [⊢ (#%datum- . s) ⇒ String]]
  [(_ . b:boolean) ≫
   -------------------------
   [⊢ (#%datum- . b) ⇒ Boolean]])

(define-typed-syntax (ann e {~datum :} τ:type) ≫
  [⊢ e ≫ e- ⇐ τ.norm]
  --------
  [⊢ e- ⇒ τ.norm])

(define-simple-macro (hash-percent-module-begin body ...)
  (#%module-begin
   (define params-hash (make-hash))
   (define resources-hash (make-hash))
   (splicing-syntax-parameterize ([params (make-variable-like-transformer #'params-hash)]
                                  [resources (make-variable-like-transformer #'resources-hash)])
     body ...)
   (define template (hash- 'Parameters params-hash
                           'Resources resources-hash))
   (provide template)
   (module+ main
     (displayln (jsexpr->string template)))))

(define-typed-syntax def
  [(_ id:id {~datum :} τ:type e:expr) ≫
   #:with id- (generate-temporary #'id)
   -------------------------
   [≻ (begin
        (define id- (ann e : τ))
        (define-syntax id
          (make-variable-like-transformer
           (⊢ id- : τ.norm))))]]
  [(_ id:id e:expr) ≫
   [⊢ e ≫ e- ⇒ τ]
   #:with id- (generate-temporary #'id)
   -------------------------
   [≻ (begin
        (define id- e-)
        (define-syntax id
          (make-variable-like-transformer
           (⊢ id- : τ))))]])

(define-typed-syntax (defparam id:id {~datum :} τ:type
                       {~or {~once {~seq #:description e_desc}}}
                       ...) ≫
  [⊢ e_desc ≫ e_desc- ⇐ String]
  #:with str (lisp-case->upper-camel-case (symbol->string (syntax-e #'id)))
  #:with sym (string->symbol (syntax-e #'str))
  #:with aws-type (type->aws-param-type #'τ)
  -------------------------
  [≻ (begin
       (hash-set! params 'sym (hash- 'Type 'aws-type
                                     'Description e_desc-))
       (define-syntax id
         (make-variable-like-transformer
          (⊢ (hash- 'Ref 'str) : τ.norm))))])

(begin-for-syntax
  (struct resource-binding (name type)
    #:property prop:procedure
    (λ (binding stx)
      ((make-variable-like-transformer
        (⊢ (hash- 'Ref '#,(resource-binding-name binding))
           : #,(resource-binding-type binding)))
       stx))))

(define-simple-macro (defresource id:id e:expr)
  #:with id- (generate-temporary #'id)
  #:with str (lisp-case->upper-camel-case (symbol->string (syntax-e #'id)))
  #:with sym (string->symbol (syntax-e #'str))
  (begin
    (define id- (ann e : Resource))
    (hash-set! resources 'sym id-)
    (define-syntax id (resource-binding 'str #'Resource))))

;; ---------------------------------------------------------------------------------------------------

(define aws:region (hash- 'Ref "AWS::Region"))
(define aws:stack-name (hash- 'Ref "AWS::StackName"))

(define-typed-syntax fn:if
  [(_ e_pred:expr e_true:expr e_false:expr) ≫
   [⊢ e_pred ≫ e_pred- ⇐ Boolean]
   [⊢ e_true ≫ e_true- ⇒ τ_true]
   [⊢ e_false ≫ e_false- ⇒ τ_false]
   #:fail-unless (type=? #'τ_true #'τ_false)
                 (string-append "both branches of an if must have the same type; "
                                "true branch has type ‘" (type->str #'τ_true) "’"
                                "while false branch has type ‘" (type->str #'τ_false) "’")
   -------------------------
   [⊢ (hash- 'Fn::If (list- e_pred- e_true- e_false-)) ⇒ τ_true]])

; TODO: make this variadic
;   NOTE: Fn::Or apparently only supports up to 10 conditions for some reason
(define (fn:or a b)
  (hash- 'Fn::Or (list- a b)))

(define-typed-syntax fn:equal?
  [(_ a:expr b:expr) ≫
   [⊢ a ≫ a- ⇒ τ_a]
   [⊢ b ≫ b- ⇒ τ_b]
   #:fail-unless (type=? #'τ_a #'τ_b)
                 (string-append "both arguments must have the same type; "
                                "first argument has type ‘" (type->str #'τ_a) "’"
                                "while second argument has type ‘" (type->str #'τ_b) "’")
   -------------------------
   [⊢ (hash- 'Fn::If (list- a- b-)) ⇒ Boolean]])

(define (fn:join strs #:separator [separator ""])
  (hash- 'Fn::Join (list- separator strs)))

; TODO: make this better and support resource attributes (see issue #3)
(define-typed-syntax fn:get-att
  [(_ {~var resource
            (local-value resource-binding?
                         #:failure-message (string-append "identifier was not bound to a resource "
                                                          "defined in this template"))}
      att:str) ≫
   #:with name (resource-binding-name (attribute resource.local-value))
   -------------------------
   [⊢ #,(syntax-property #'(hash- 'Fn::GetAtt (list- 'name 'att))
                         'disappeared-use (syntax-local-introduce #'resource))
      ⇒ String]])

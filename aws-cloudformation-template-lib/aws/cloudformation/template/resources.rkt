#lang turnstile

(require (for-syntax syntax/parse/experimental/template
                     threading)
         aws/cloudformation/template/base
         (only-in racket/contract the-unsupplied-arg))

(begin-for-syntax
  (define keyword->symbol
    (λ~> syntax-e keyword->string lisp-case->upper-camel-case string->symbol
         (datum->syntax #f _))))

(define-syntax-parser define-resource-type
  #:literals [=>]
  [(_ id:id
      #:aws-type aws-type:str
      { {~or {~seq kw_req:keyword
                   {~optional {~seq => sym_req:id}
                              #:defaults ([sym_req (keyword->symbol #'kw_req)])}
                   τ_req:type}
             [kw_opt:keyword
              {~optional {~seq => sym_opt:id}
                         #:defaults ([sym_opt (keyword->symbol #'kw_opt)])}
              τ_opt:type]}
        ... })
   #:with [arg_req ...] (generate-temporaries (attribute kw_req))
   #:with [arg_opt ...] (generate-temporaries (attribute kw_opt))
   (template
    (begin
      (define (id {?@ kw_req arg_req} ... {?@ kw_opt [arg_opt the-unsupplied-arg]} ...)
        (hash 'Type 'aws-type
              'Properties (make-immutable-hash
                           (append (list (cons {?@ 'sym_req arg_req}) ...)
                                   (filter (λ (pair) (not (eq? the-unsupplied-arg (cdr pair))))
                                           (list (cons {?@ 'sym_opt arg_opt}) ...))))))
      (provide (typed-out [id : (-> {?@ kw_req τ_req} ... [kw_opt τ_opt] ... Resource)]))))])

(define-resource-type aws:elastic-load-balancing:load-balancer
  #:aws-type "AWS::ElasticLoadBalancing::LoadBalancer"
  { [#:access-logging-policy (Record [#:emit-interval Number]
                                     #:enabled Boolean
                                     #:s3-bucket-name String
                                     [#:s3-bucket-prefix String])]
    [#:app-cookie-stickiness-policy (List (Record #:cookie-name String
                                                  #:policy-name String))]
    [#:availability-zones (List String)]
    [#:connection-draining-policy (Record #:enabled Boolean
                                          [#:timeout Number])]
    [#:connection-settings (Record #:idle-timeout Number)]
    [#:cross-zone Boolean]
    [#:health-check (Record #:healthy-threshold String
                            #:interval String
                            #:target String
                            #:timeout String
                            #:unhealthy-threshold String)]
    [#:instances (List String)]
    [#:lb-cookie-stickiness-policy => LBCookieStickinessPolicy (List (Record [#:cookie-expiration-period String]
                                                                             #:policy-name String))]
    [#:load-balancer-name String]
    #:listeners (List (Record #:instance-port Number
                              [#:instance-protocol String]
                              #:load-balancer-port Number
                              [#:policy-names (List String)]
                              #:protocol String))
    [#:scheme String]
    [#:security-groups (List String)]
    [#:subnets (List String)]
    [#:tags (List (Record #:key String
                          #:value String))]})

(define-resource-type aws:ecs:service
  #:aws-type "AWS::ECS::Service"
  { [#:cluster Resource]
    [#:deployment-configuration (Record [#:maximum-percent Number]
                                        [#:minimum-healthy-percent Number])]
    #:desired-count Number
    [#:load-balancers (List (Record #:container-name String
                                    #:container-port Number
                                    #:load-balancer-name Resource
                                    [#:target-group-arn String]))]
    [#:role String]
    #:task-definition Resource })

(define-resource-type aws:ecs:task-definition
  #:aws-type "AWS::ECS::TaskDefinition"
  { #:container-definitions (List (Record [#:command (List String)]
                                          [#:cpu Number]
                                          [#:disable-networking Boolean]
                                          [#:dns-search-domains (List String)]
                                          [#:dns-servers (List String)]
                                          [#:docker-security-options (List String)]
                                          [#:entry-point (List String)]
                                          [#:environment (List (Record #:name String
                                                                       #:value String))]
                                          [#:essential Boolean]
                                          [#:extra-hosts (List (Record #:hostname String
                                                                       #:ip-address String))]
                                          [#:hostname String]
                                          #:image String
                                          [#:links (List String)]
                                          #:memory Number
                                          [#:mount-points (List (Record #:container-path String
                                                                        #:source-volume String
                                                                        [#:read-only Boolean]))]
                                          #:name String
                                          [#:port-mappings (List (Record #:container-port Number
                                                                         [#:host-port Number]
                                                                         [#:protocol String]))]
                                          [#:privileged Boolean]
                                          [#:readonly-root-filesystem Boolean]
                                          [#:ulimits (List (Record #:hard-limit Number
                                                                   [#:name String]
                                                                   #:soft-limit Number))]
                                          [#:user String]
                                          [#:volumes-from (List (Record #:source-container String
                                                                        [#:read-only Boolean]))]
                                          [#:working-directory String]))
    [#:family String]
    [#:task-role-arn String]
    [#:volumes (List (Record #:name String
                             [#:host (Record [#:source-path String])]))] })

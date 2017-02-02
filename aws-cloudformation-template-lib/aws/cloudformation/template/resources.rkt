#lang curly-fn turnstile

(require (for-syntax syntax/parse/experimental/template
                     threading)
         aws/cloudformation/template/base
         (only-in racket/contract the-unsupplied-arg)
         racket/function)

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
        ... }
      {~optional {~seq #:transform transformer}
                 #:defaults ([transformer #'identity])})
   #:with [arg_req ...] (generate-temporaries (attribute kw_req))
   #:with [arg_opt ...] (generate-temporaries (attribute kw_opt))
   (template
    (begin
      (define (id {?@ kw_req arg_req} ... {?@ kw_opt [arg_opt the-unsupplied-arg]} ...)
        (hash 'Type 'aws-type
              'Properties (transformer
                           (make-immutable-hash
                            (append (list (cons {?@ 'sym_req arg_req}) ...)
                                    (filter (λ (pair) (not (eq? the-unsupplied-arg (cdr pair))))
                                            (list (cons {?@ 'sym_opt arg_opt}) ...)))))))
      (provide (typed-out [id : (-> {?@ kw_req τ_req} ... [kw_opt τ_opt] ... Resource)]))))])

(define (rename-key hsh old-key new-key)
  (let ([v (hash-ref hsh old-key)])
    (hash-remove (hash-set hsh new-key v) old-key)))

(define (hash-ref-transform hsh key proc)
  (if (hash-has-key? hsh key)
      (hash-update hsh key proc)
      hsh))

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
                                          [#:log-configuration (Record #:log-driver String
                                                                       [#:options (Dict String)])]
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

(define-resource-type aws:iam:policy
  #:aws-type "AWS::IAM::Policy"
  { [#:groups (List String)]
    #:policy-document (Record #:statement (List (Record #:effect String
                                                        #:action (List String)
                                                        #:resource (List String))))
    #:policy-name String
    [#:roles (List String)]
    [#:users (List String)]})

(define-resource-type aws:route53:record-set
  #:aws-type "AWS::Route53::RecordSet"
  { [#:alias-target (Record #:dns-name String
                            [#:evaluate-target-health Boolean]
                            #:hosted-zone-id String)]
    [#:comment String]
    [#:failover String]
    [#:geo-location String]
    [#:health-check-id String]
    [#:hosted-zone-id String]
    [#:hosted-zone-name String]
    #:name String
    [#:region String]
    [#:resource-records (List String)]
    [#:set-identifier String]
    [#:ttl => TTL String]
    #:type String
    [#:weight Number] }
  #:transform (λ (properties)
                (hash-ref-transform properties 'AliasTarget
                                    #{rename-key % 'DnsName 'DNSName})))

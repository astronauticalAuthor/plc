


;; Parsed expression datatypes


(define id-literal
  (lambda (arg)
    (or (number? arg) (char? arg) (string? arg) (boolean? arg) (vector? arg) (list? arg) (symbol? arg) (pair? arg))))


(define-datatype expression expression?
  [var-exp
    (id symbol?)]  ; this is from eval-exp.  should help turn the expression into the evaluated expression
  [lit-exp
    (id id-literal)]
  [lambda-exp
    (id (lambda (x) (or (symbol? x) (list-of var-exp))))
    (body (list-of expression?))]
  [inf-arg-lambda-exp
    (id symbol?)
    (body (list-of expression?))]
  [pair-arg-lambda-exp
    (id pair?)
    (body (list-of expression?))]
  [ref-lambda-exp
    (id (list-of symbol?))
    (refs (list-of number?))
    (bodies (list-of expression?))]   ;;  ref lambda here
  [if-exp
    (test-exp expression?)
    (then-exp expression?)
    (else-exp expression?)]
  [if-one-exp
    (test-exp expression?)
    (then-exp expression?)]
  [set!-exp
    (id symbol?)
    (val expression?)]
  [named-let-exp
    (name symbol?)
    (vars list?)
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [let-exp
    (vars list?)
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss list?)
    (bodiess (list-of (list-of expression?)))
    (letrec-bodies (list-of expression?))]
  [app-exp
    (rator expression?)
    (rand (list-of expression?))]
  [let*-exp
    (vars list?)
    (exps (list-of expression?))
    (bodies (list-of expression?))]
  [begin-exp
    (bodies (list-of expression?))]
  [and-exp
    (bodies (list-of expression?))]
  [or-exp
    (bodies (list-of expression?))]
  [while-exp
    (test expression?)
    (bodies (list-of expression?))]
  [cond-exp
    (tests (list-of expression?))
    (bodies (list-of expression?))]
  [case-exp
    (key expression?)
    (tests (list-of expression?))
    (bodies (list-of expression?))]
  [define-exp
    (var symbol?)
    (exp expression?)])

(define-datatype environment environment?
  (empty-env-record)  ;Exception in closure:   Bad env field (environment? (empty-env-record)) => #f
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))  
   (env environment?))  ; was environment

  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss list?)
    (bodiess (list-of (list-of expression?)))
    (env environment?)] ; was environment
  )


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (vars (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)] ; was environment
  [inf-closure
    (var symbol?)
    (bodies (list-of expression?))
    (env environment?)] ; was environment
  [pair-closure
    (vars pair?)
    (bodies (list-of expression?))
    (env environment?)] ; was environment
  [ref-closure
    (ids (list-of symbol?))
    (refs (list-of number?))
    (bodies (list-of expression?))
    (env environment?)]
  [continuation-proc 
    (k continuation?)]
  )

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))


(define-datatype continuation continuation?
  [init-k]
  [rator-k 
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [rands-k 
    (rator-val scheme-value?)
    (k continuation?)]
  [if-k
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)]
  [no-else-if-k
    (then-exp expression?)
    (env environment?)
    (k continuation?)]
  [set!-exp-k 
    (env environment?)
    (id symbol?)
    (k continuation?)]
  [map-car-k
    (cdr-ls (list-of scheme-value?))
    (proc-cps scheme-value?)
    (k continuation?)]
  [map-cons-k 
    (car-ls scheme-value?)
    (k continuation?)]

  [eval-while-rands-k1
    (test-exp expression?)
    (bodies (list-of expression?))
    (env environment?)
    (k continuation?)]
  [eval-while-rands-k2 
      (test-exp expression?)
      (bodies (list-of expression?))
      (env environment?)
      (k continuation?)]
  [define-k 
      (id symbol?)
      (env environment?)
      (k continuation?)]
  [eval-bodies-k
      (exps (list-of expression?))
      (env environment?)
      (k continuation?)]
  [eval-bodies-rands-k
      (proc-val scheme-value?)
      (k continuation?)]
  ; [eval-begin-rator-k 
  ;     (rands (list-of expression?))
  ;     (env environment?)
  ;     (k continuation?)]
  ; [eval-begin-rands-k 
  ;     (proc-value scheme-value?)
  ;     (k continuation?)]

    )
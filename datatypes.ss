
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp
    (id symbol?)]  ; this is from eval-exp.  should help turn the expression into the evaluated expression
  [lit-exp
    (literal (lambda (x) #t))]
  [lambda-exp
    (id (lambda (x) (or (symbol? x) (list-of var-exp))))
    (body (list-of expression?))]
  [if-exp
    (condition expression?)
    (result (list-of expression?))]
  [set!-exp
    (id symbol?)
    (val expression?)]
  [named-let-exp
    (name symbol?)
    (arguments list?)
    (body (list-of expression?))]
  [let-exp
    (arguments list?)
    (body (list-of expression?))]
  [letrec-exp
    (arguments list?)
    (body (list-of expression?))]
  [app-exp
    (rator expression?)
    (rand (list-of expression?))]
  [let*-exp
    (arguments list?)
    (body (list-of expression?))])

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (vars (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)])
	 
(define-datatype bool-val bool-val?
  [true-exp #t])
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
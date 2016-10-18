; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id
          (lambda (x) x)
          (lambda () 
            (apply-env init-env id (lambda (x) x) 
              (lambda () (error 'apply-env "variable ~s is not bound" id)))))]
      [let-exp (vars exps bodies)
        (let ([new-env (extend-env vars (eval-rands exps env) env)])
          (eval-bodies bodies new-env))]
      [if-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env)
            (eval-exp else-exp env))]
      [if-one-exp (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
      [set!-exp (vars exps)
        (list '())]
      [named-let-exp (name vars exps bodies)
        (list '())]
      [letrec-exp (vars exps bodies)
        (list '())]
      [let*-exp (vars exps bodies)
        (let ([new-env (extend-env vars (eval-rands exps env) env)])
          (eval-bodies bodies new-env))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)] [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [lambda-exp (vars bodies)
        (closure vars bodies env)]
      [inf-arg-lambda-exp (var body)
        (inf-closure var body env)]
      [pair-arg-lambda-exp (vars body)
        (pair-closure vars body env)]
      [else (error 'eval-exp "bad expression case ~s" exp)]
      )))

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [begin-exp (bodies)
        (app-exp (lambda-exp '() bodies) '())]
      [and-exp (bodies)
        (if (not (null? bodies))
          (if-exp (car bodies) (syntax-expand (and-exp (cdr bodies))) (lit-exp #f))
          (lit-exp #t))]
      [or-exp (bodies)
        (if (not (null? bodies))
          (if-exp (car bodies) (lit-exp #t) (syntax-expand (or-exp (cdr bodies))))
          (lit-exp #f))]
      ; [let*-exp (vars exps bodies)
      ;   ; (if (not (null? vars))
      ;   ;   (let-exp (list (car vars)) (list (car exps)) (syntax-expand (let*-exp (cdr vars) (cdr exps) bodies)))
      ;   ;   bodies
      ;   ;   )
      ;   (cond
      ;     [(and (not (null? vars)) (not (null? (cdr vars)))) (list (let-exp (list (car vars)) (list (car exps)) (syntax-expand (let*-exp (cdr vars) (cdr exps) bodies))))]
      ;     [(and (not (null? vars)) (null? (cdr vars))) (let-exp (list (car vars)) (list (car exps)) bodies)]
      ;     [else bodies]

      ;     )



      ;   ]



      [else exp])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (e)
        (eval-exp e env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (vars bodies env)
        (eval-bodies bodies (extend-env vars args env))]
      [inf-closure (var bodies env)
        (eval-bodies bodies (extend-env (list var) (list args) env))]
      [pair-closure (vars bodies env)
        (let loop ([old-vars vars] [new-vars '()] [old-args args] [new-args '()])
          (if (not (pair? old-vars))
            (eval-bodies bodies (extend-env (reverse (cons old-vars new-vars)) (reverse (cons old-args new-args)) env))
            (loop (cdr old-vars) (cons (car old-vars) new-vars) (cdr old-args) (cons (car old-args) new-args))))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / zero? add1 sub1 not cons car cdr null? < <= > >= = list append assq assv assoc equal? eq? eqv? atom? length list->vector
                              list->string list->fxvector vector make-vector vector-ref list-ref vector? number? symbol? set-car! set-cdr! vector-set! display
                              newline cadr caar cdar cddr caaar caadr cadar cdaar cddar cdadr caddr cdddr list? procedure? pair? vector->list void map apply begin quotient))
(define bool-vals '(#t #f))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(procedure?) (proc-val? (car args))]  ; not working
      [(pair?) (pair? (car args))]
      [(zero?) (eq? 0 (car args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(not) (not (car args))]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (car (car args))]
      [(cdr) (cdr (car args))]
      [(null?) (null? (car args))]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(=) (apply = args)]
      [(list) args]
      [(list?) (list? (car args))]
      [(append) (append (1st args) (2nd args))]
      [(assq) (assq (car args) (cdr args))]
      [(assv) (assv (car args) (cdr args))]
      [(assoc) (assoc (car args) (cdr args))]
      [(equal?) (equal? (car args) (cadr args))]
      [(eq?) (eq? (car args) (cadr args))]
      [(eqv?) (eqv? (car args) (cadr args))]
      [(atom?) (atom? (car args))]
      [(length) (length (car args))]
      [(list->vector) (list->vector (car args))]
      [(list->string) (list->string (car args))]
      [(list->fxvector) (list->fxvector (car args))]
      [(vector->list) (vector->list (car args))]
      [(vector) (apply vector args)]
      [(make-vector) (if (= (length args) 1) (make-vector (1st args)) (make-vector (1st args) (2nd args)))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(list-ref) (list-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (if (= (length args) 1) (display (1st args)) (display (1st args) (2nd args)))]
      [(newline) (newline)]
      [(cadr) (cadr (car args))]
      [(caar) (caar (car args))]
      [(cdar) (cdar (car args))]
      [(cddr) (cddr (car args))]
      [(caaar) (caaar (car args))]
      [(caadr) (caadr (car args))]
      [(cadar) (cadar (car args))]
      [(cdaar) (cdaar (car args))]
      [(caddr) (caddr (car args))]
      [(cdadr) (cdadr (car args))]
      [(cddar) (cddar (car args))]
      [(cdddr) (cdddr (car args))]
      [(void) (void)]
      [(map) (map (car args) (cadr args))]
      [(apply) (apply (car args) (cadr args))]
      [(quotient) (quotient (car args) (cadr args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda(x)
    (top-level-eval (syntax-expand (parse-exp x)))
  ))

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin (eval-exp (car bodies) env)
        (eval-bodies (cdr bodies) env)))))


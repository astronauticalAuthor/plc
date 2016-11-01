(define apply-k
  (trace-lambda continuationing? (k val)
    (cases continuation k
      [init-k () val]
      [rator-k (rands env k)
        (eval-rands rands env (rands-k val k))]
      [rands-k (rator-val k)
        (apply-proc rator-val val k)]
      [if-exp-k (then-exp else-exp env k)
        (if val
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
      [if-one-exp-k (then-exp env k)
        (if val
          (eval-exp then-exp env k))]

      [let-rands-k (vars bodies env k)
        (eval-bodies bodies (extend-env vars val env) k)])))


; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env) (init-k))))

; eval-exp is the main component of the interpreter
(define eval-exp
  (trace-lambda evaling? (exp cell k)
    ; (let ([env (deref cell)])
    (let ([env cell])
      (cases expression exp
        [lit-exp (datum) (apply-k k datum)]
        [var-exp (id)
          (apply-env env id 
            k  ; (lambda (x) x)
            (lambda () 
              (apply-env-ref global-env id (lambda (x) x) 
                (lambda () (error 'apply-env "variable ~s is not bound" id)))))]
        [let-exp (vars exps bodies)
          (let ([new-env (extend-env vars (eval-rands exps env (rands-k bodies k)) env)])
            (eval-bodies bodies new-env k))]
        [if-exp (test-exp then-exp else-exp)
          (if (eval-exp test-exp env k)
              (eval-exp then-exp env k)
              (eval-exp else-exp env k))]
        [if-one-exp (test-exp then-exp)
          (if (eval-exp test-exp env k)
              (eval-exp then-exp env k))]
        [set!-exp (var exp)
          (mod-env-set! env var (eval-bodies (list exp) env k))]
        [letrec-exp (proc-names idss bodiess letrec-bodies)
          (eval-bodies letrec-bodies
            (extend-env-recursively proc-names idss bodiess env) k)]  ; redo for CPS
        [let*-exp (vars exps bodies)
          (let ([new-env (extend-env vars (eval-rands exps env (rands-k bodies k)) env)])
            (eval-bodies bodies new-env k))]
        [app-exp (rator rands)
          (let ([proc-value (eval-exp rator env (rator-k rands env k))] [args (eval-rands rands env (rands-k rands k))])
            (apply-proc proc-value args k))
          ; (eval-exp rator env (rator-k rands env k))
          ]
        [lambda-exp (vars bodies)
          (apply-k k (closure vars bodies env))]
        [inf-arg-lambda-exp (var body)
          (inf-closure var body env)]
        [pair-arg-lambda-exp (vars body)
          (pair-closure vars body env)]
        [ref-lambda-exp (ids refs bodies)
          (ref-closure ids refs bodies env)]
        [while-exp (test bodies)
          (eval-exp (if-one-exp test
                      (app-exp (lambda-exp '() (append bodies (list (while-exp test bodies)))) '())
                      ) env k)]
        [define-exp (var exp)
          (set! global-env (extend-env (list var) (list (eval-exp exp env k)) global-env))]
        [else (error 'eval-exp "bad expression case ~s" exp)]
        ))))

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op)
        (apply-prim-proc op args k)]
      [closure (vars bodies env)
        (eval-bodies bodies (extend-env vars args env) k)]
      [inf-closure (var bodies env)
        (eval-bodies bodies (extend-env (list var) (list args) env) k)]
      [pair-closure (vars bodies env)
        (let loop ([old-vars vars] [new-vars '()] [old-args args] [new-args '()])
          (if (not (pair? old-vars))
            (eval-bodies bodies (extend-env (reverse (cons old-vars new-vars)) (reverse (cons old-args new-args)) env) k)
            (loop (cdr old-vars) (cons (car old-vars) new-vars) (cdr old-args) (cons (car old-args) new-args))))]
      [ref-closure (ids refs bodies env)
              ; (begin 
              ;   (display "id: ") (display ids) (newline)
              ;   (display "refs: ") (display refs) (newline)
              ;   (display "body: ") (display bodies) (newline)
              ;   (display "env: ") (display proc-env) (newline)
                (let* ([ref-vals (map (lambda (x) (list-ref ids x)) refs)]
                        [non-ref-vals (letrec ([looper (lambda (ls refs count res)
                                                    (cond
                                                      [(null? ls) result]  ; if there are no more non-ref, ret result
                                                      [(null? refs) 
                                                       (looper (cdr ls) refs (+ count 1) (append result (list (car ls))))] ; append if no more references
                                                      [(= count (car refs))
                                                       (looper (cdr ls) (cdr refs) (+ count 1) result)] ; do not include references values
                                                      [else
                                                        (looper (cdr ls) refs (+ count 1) (append result (list (car ls))))] ; in case the value is not in the place currently used as (car refs)
                                                ))])
                                      (looper ids refs 0 '()))]
                        ) (display "rigity wrecked"))
                ; )
              ]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

; (define ref-nonref
;   (lambda (ls refs)
;     (letrec looper (lambda (ls refs count res)
;             (cond
;               [(null? ls) (reverse result)]
;               [(null? refs) 
;                (looper (cdr ls) refs (+ count 1) (cons (car ls) result))]
;               [(= count (car refs))
;                (looper (cdr ls) (cdr refs) (+ count 1) result)]
;               [else
;                 (looper (cdr ls) refs (+ count 1) (cons (car ls) result))]
;         ) 
;     (looper ls refs 0 '())))))

; the shifting of the list would cause potential problems
; (define remove-index
;   (lambda (ls index)
;     (cond 
;       [(> index (length ls)) ls] ; may have already been removed
;       [])))

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [if-exp (test-exp then-exp else-exp)
        (if-exp (syntax-expand test-exp) (syntax-expand then-exp) (syntax-expand else-exp))]
      [if-one-exp (test-exp then-exp)
        (if-one-exp (syntax-expand test-exp) (syntax-expand then-exp))]
      [lambda-exp (id bodies)
        (lambda-exp id (map syntax-expand bodies))]
      [ref-lambda-exp (ids refs bodies)
        (ref-lambda-exp ids refs (map syntax-expand bodies))]
      [inf-arg-lambda-exp (id bodies)
        (inf-arg-lambda-exp id (map syntax-expand bodies))]
      [pair-arg-lambda-exp (id bodies)
        (pair-arg-lambda-exp id (map syntax-expand bodies))]
      [let-exp (vars exps bodies)
        (let-exp vars (map syntax-expand exps) (map syntax-expand bodies))]
      [named-let-exp (name vars exps bodies)
        (letrec-exp (list name) (list vars) (list (map syntax-expand bodies)) (list (app-exp (var-exp name) (map syntax-expand exps))))]
      [begin-exp (bodies)
        (app-exp (lambda-exp '() (map syntax-expand bodies)) '())]
      [and-exp (bodies)
        (if (not (null? bodies))
          (if-exp (car bodies) (syntax-expand (and-exp (cdr bodies))) (lit-exp #f))
          (lit-exp #t))]
      [or-exp (bodies)
        (if (not (null? bodies))
          (if-exp (syntax-expand (car bodies)) (lit-exp #t) (syntax-expand (or-exp (cdr bodies))))
          (lit-exp #f))]
      [let*-exp (vars exps bodies)
        (cond
          [(and (not (null? vars)) (not (null? (cdr vars))))
            (let-exp (list (car vars)) (list (car exps)) (list (syntax-expand (let*-exp (cdr vars) (cdr exps) bodies))))]
          [(and (not (null? vars)) (null? (cdr vars)))
            (let-exp vars exps bodies)]
          [else bodies])]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (letrec-exp proc-names idss (map (lambda (x) (map syntax-expand x)) bodiess) (map syntax-expand letrec-bodies))]
      [while-exp (test bodies)
        (while-exp (syntax-expand test) (map syntax-expand bodies))]
      [cond-exp (tests bodies)
        (cond
          [(null? tests) (lit-exp (void))]
          [(eqv? 'else (cadar tests)) (syntax-expand (car bodies))]
          [(not (null? tests)) (if-exp (syntax-expand (car tests)) (syntax-expand (car bodies)) (syntax-expand (cond-exp (cdr tests) (cdr bodies))))])]

      [case-exp (key tests bodies)
        (cond
          [(null? (cdr tests)) (syntax-expand (car bodies))]
          [(eqv? 'else (cadar tests)) (car bodies)]
          [else (if-exp (app-exp (var-exp 'member) (list key (car tests)))
                        (car bodies)
                        (syntax-expand (case-exp key (cdr tests) (cdr bodies))))])]
      [set!-exp (var exp)
        (set!-exp var (syntax-expand exp))]
      [define-exp (var exp)
        (define-exp var (syntax-expand exp))]
      [else exp])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (map (lambda (e)
        (eval-exp e env k)) rands)))

(define *prim-proc-names* '(+ - * / zero? add1 sub1 not cons car cdr null? < <= > >= = list append assq assv assoc equal? eq? eqv? atom? length list->vector
                              list->string list->fxvector vector make-vector vector-ref list-ref vector? number? symbol? set-car! set-cdr! vector-set! display
                              newline cadr caar cdar cddr caaar caadr cadar cdaar cddar cdadr caddr cdddr list? procedure? pair? vector->list void map apply begin quotient member
                              list-tail))
(define bool-vals '(#t #f))

; copy init-env here
(define make-init-env  ; made to show that the global is just the init-env.  Will need to change parts to mirror the adding to the init-env
  (lambda ()
      (extend-env            ; procedure names.  Recall that an environment associates
          *prim-proc-names*   ;  a value (not an expression) with an identifier.
          (map prim-proc      
              *prim-proc-names*)
          (empty-env)
          (init-k))))
      ; (empty-env)))

(define init-env (make-init-env))

(define global-env init-env)

(define reset-global-env
  (lambda () 
    (set! global-env (make-init-env))))  ; make-init-env is just copy-pasta of init-env

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc ; convert to cps
  (trace-lambda primming? (prim-proc args k)
    (case prim-proc
      [(call/cc) (apply-proc (car args) (list (k-proc k)) k)]
      [(apply) (apply-proc (car args) (cadr args) k)]
      [(map) (map-proc-cps (lambda (x k)
                                    (apply-proc-cps (1st args) (list x) k)) (2nd args) (lambda (mapped-args)
                                                                                            (apply-k k mapped-args)))]
      [else ; the above is applying k in a different way
            (apply-k k
                (case prim-proc
                  [(+) (apply-k k (apply + args))]
                  [(-) (apply - args)]
                  [(*) (apply * args)]
                  [(/) (apply / args)]
                  [(procedure?) (proc-val? (car args))]
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
                  [(assq) (assq (car args) (cadr args))]
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
                  
                  
                  [(quotient) (quotient (car args) (cadr args))]
                  [(member) (member (car args) (cadr args))]
                  [(list-tail) (list-tail (car args) (cadr args))]
                  
                  [else (error 'apply-prim-proc 
                        "Bad primitive procedure name: ~s" 
                        prim-proc)]))
            ])))

(define map-proc-cps
    (lambda (proc-cps ls k)
        (if (null? ls)
            (apply-k k '())
            (proc-cps (car ls) (lambda (mapped-car)
                                    (map-proc-cps (cdr ls) (lambda (mapped-cdr)
                                                                (apply-k k (cons mapped-car mapped-cdr)))))))))

; (define map-proc  
;   (lambda (proc ls)
;     (if (null? ls)
;       '()
;       (cons (apply-proc proc (list (car ls))) (map-proc proc (cdr ls))))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x)
    (let ([eval-result (top-level-eval (syntax-expand (parse-exp x)))])
            eval-result)
  ))

; (define eval-bodies
;   (lambda (bodies env)
;     (if (null? (cdr bodies))
;       (eval-exp (car bodies) env)
;       (begin (eval-exp (car bodies) env)
;         (eval-bodies (cdr bodies) env)))))

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env k)
        (eval-exp (car bodies) env (lambda (eval-car)
                                        (eval-bodies-cps (cdr bodies) env k))))))
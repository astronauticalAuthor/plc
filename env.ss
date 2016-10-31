; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

;  implement stuff for cell
(define make-cell
  (lambda (val)
    (cons val 'this-is-a-cell)))
(define cell-ref car)
(define cell-set! set-car!)
(define cell?
  (lambda (cel)
    (and (pair? cel) (eq? (cdr cel) 'this-is-a-cell))))

(define empty-env
  (lambda ()
    (make-cell (empty-env-record))))

(define extend-env
  (lambda (syms vals env)
    (make-cell (extended-env-record syms vals env))))

(define extend-env-recursively
  (lambda (proc-names idss bodiess old-env)
    (make-cell (recursively-extended-env-record
        proc-names idss bodiess old-env))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))



(define deref cell-ref)

(define apply-env
  (lambda (cell sym succeed fail)
    (apply-env-ref cell sym succeed fail)))
  ; (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
  ;   (cases environment env
  ;     [empty-env-record ()
  ;       (fail)]
  ;     [extended-env-record (syms vals env)
  ;       	(let ((pos (list-find-position sym syms)))
  ;             	  (if (number? pos)
  ;       	      (succeed (list-ref vals pos))
  ;       	      (apply-env env sym succeed fail)))]

  ;     [recursively-extended-env-record (procnames idss bodiess old-env)
  ;       (let ([pos (list-find-position sym procnames)])
  ;           (if (number? pos)
  ;               (let ([id (list-ref idss pos)])
  ;                 (cond
  ;                   [(symbol? id) (inf-closure id (list-ref bodiess pos) env)]
  ;                   [(list? id) (closure id (list-ref bodiess pos) env)]
  ;                   [(pair? id) (pair-closure id (list-ref bodiess pos) env)]))
  ;               (apply-env old-env sym succeed fail)))]
  ;     )))


(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (begin 
      ; (display "here I am, once again")
    (cases environment env
      [empty-env-record ()
        (fail)]
      [extended-env-record (syms vals env)
          (let ((pos (list-find-position sym syms)))
                  (if (number? pos)
                      (succeed (list-ref vals pos))
                      (apply-env-ref env sym succeed fail)))]

      [recursively-extended-env-record (procnames idss bodiess old-env)
        (let ([pos (list-find-position sym procnames)])
            (if (number? pos)
                (let ([id (list-ref idss pos)])
                  (cond
                    [(symbol? id) (inf-closure id (list-ref bodiess pos) env)]
                    [(list? id) (closure id (list-ref bodiess pos) env)]
                    [(pair? id) (pair-closure id (list-ref bodiess pos) env)]))
                (apply-env-ref old-env sym succeed fail)))]
      [else (display 'mr-poopy-butt-hole)]
      ))))

(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

(define mod-env-set!
  (lambda (cell val new-val)
      (cases environment (deref cell)
          [empty-env-record () (mod-global-env-set! val new-val)]
          [extended-env-record (syms vals env)
            (let ([pos (list-find-position val syms)] )
                (if (number? pos)
                    (list-set! vals pos new-val)
                    (mod-env-set! env val new-val)))]
          [recursively-extended-env-record (procnames idss bodiess old-env)
              (let loop ([count 0])
                  (if (>= count (length idss))
                      (mod-env-set! old-env val new-val)
                      (let ([pos (list-find-position val (list-ref idss count))])
                          (if (number? pos)
                              (list-set! vals pos new-val)
                              (loop (+ count 1))))))]
          [else (display 'mr-poopy-butt-hole)])))


(define mod-global-env-set!
    (lambda (val new-val)
        (cases environment (deref global-env)
            [empty-env-record () (eopl:error 'apply-env "you are trying to set to an empty environment")]
            [extended-env-record (syms vals env)
              (let ([pos (list-find-position val syms)])
                  (if (number? pos)
                    (list-set! vals pos new-val)
                    (eopl:error 'mod-global-env-set! "you are trying to set to an empty environment")))]
            [recursively-extended-env-record (procnames idss bodiess old-env)
              (let loop ([count 0])
                  (if (>= count (length idss))
                      (eopl:error 'mod-global-env-set! "you are trying to set to an empty environment")
                      (let ([pos (list-find-position val (list-ref idss count))])
                          (if (number? pos)
                              (list-set! vals pos new-val)
                              (loop (+ count 1))))))]
            [else (display 'mr-poopy-butt-hole)])))


;  this will largely be a copy of apply-env-ref for test purposes
; (define mod-env-set!
;   (lambda (cell val new-val)
;       (cases environment (deref cell)
;           [empty-env-record () 
;             (eopl:error 'apply-env "you are trying to set to an empty environment")]
;           [extended-env-record (syms vals env)
;             (let ((pos (list-find-position sym syms)))
;                     (if (number? pos)
;                         (set-box! (list-ref vals pos) (if (box? new-val)  ; be sure to set the new environment to the value from the box or just the value
;                                                           (unbox new-val)
;                                                           new-val))
;                         (mod-env-set! cell val new-val)))])))  ; need to finish this.  You need to apply the environment to an error if empty, and then make a case for the extended env-record and rec-extend-env


; ;  this will largely be a copy of apply-env-ref for test purposes
; (define mod-env-define
;   (lambda (val new)
;       (cases environment (deref global-env)
;           [empty-env-record () 
;               (apply-env global-env val succeed fail)]
;           [extended-env-record (sym val cell)
;               ()]
;           )))
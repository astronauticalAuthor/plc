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
    ; (make-cell (empty-env-record))))
    (empty-env-record)))

(define extend-env
  (trace-lambda extend-env? (syms vals env k)
    ; (make-cell (extended-env-record syms vals env))))
    (apply-k k (extended-env-record syms vals env))))

(define extend-env-recursively
  (trace-lambda extend-env-recursively? (proc-names idss bodiess old-env kontinuation)
    ; (make-cell (recursively-extended-env-record
    ;     proc-names idss bodiess old-env))))
    (apply-k kontinuation (recursively-extended-env-record proc-names idss bodiess old-env))))

(define list-find-position
  (trace-lambda list-find-position? (sym los k)
    (list-index (lambda (xsym k) (apply-k k (eqv? sym xsym)) los k) los k)))

(define list-index
  (trace-lambda list-index? (pred-cps ls k)
    (cond
     [(null? ls) (begin 
              (display k) 
              (apply-k k #f))]
     ; ((pred (car ls)) 0)
   ;   (else (let ((list-index-r (list-index pred (cdr ls))))
	  ;    (if (number? list-index-r)
		 ; (+ 1 list-index-r)
		 ; #f)
   ;     )
   ;   )
      [else (pred-cps (car ls) (list-index-cps pred-cps ls k))]
     )))



(define deref cell-ref)

(define apply-env
  (trace-lambda apply-env? (env sym succeed fail)
    (apply-env-ref env sym succeed fail)))

(define apply-env-ref
  (trace-lambda applying-env-ref? (env sym succeed fail)
    (cases environment env
      [empty-env-record ()
        (fail)]
      [extended-env-record (syms vals env)
          ; (let ((pos (list-find-position sym syms )))
          ;         (if (number? pos)
          ;             (succeed (list-ref vals pos))
          ;             (apply-env-ref env sym succeed fail)))
          ; (list-find-position sym syms (lambda (vals sym env k fail)
          ;                                   (if (number? sym)
          ;                                     #f
          ;                                     #t)))
          (list-find-position sym syms (list-find-pos-k vals sym env succeed fail))
          ]

      [recursively-extended-env-record (procnames idss bodiess old-env)
        ; (let ([pos (list-find-position sym procnames)])
        ;     (if (number? pos)
        ;         (let ([id (list-ref idss pos)])
        ;           (cond
        ;             [(symbol? id) (apply-k k (inf-closure id (list-ref bodiess pos) env))]
        ;             [(list? id) (apply-k k (closure id (list-ref bodiess pos) env))]
        ;             [(pair? id) (apply-k k (pair-closure id (list-ref bodiess pos) env))]))
        ;         (apply-env-ref old-env sym succeed fail)))
        (list-find-position sym ids (list-find-rec-k bodies sym env old-env succeed fail))
        ]
      [else (display 'mr-poopy-butt-hole)]
      )))

;  sets a value val at position k in a list
(define (list-set! list pos val k)
    (if (zero? pos)
        (apply-k k (set-car! list val))
        (list-set! (cdr list) (- pos 1) val k)))

(define mod-env-set!
  (trace-lambda mod-env-set? (cell val new-val k)
      (cases environment cell
          [empty-env-record () (mod-global-env-set! val new-val global-env k)]
          [extended-env-record (syms vals env)
            ; (let ([pos (list-find-position val syms)])
            ;     (if (number? pos)
            ;         (list-set! vals pos new-val)
            ;         (mod-env-set! env val new-val)))
            ; first find the position
            ; set the new env to whatever vals is
            (list-find-position val syms)
            ]
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
    (trace-lambda mod-global-env-set? (val new-val env k)
        (cases environment env
            [empty-env-record () (eopl:error 'apply-env "you are trying to set to an empty environment")]
            [extended-env-record (syms vals env)
              (let ([pos (list-find-position val syms)])
                  (if (number? pos)
                    (list-set! vals pos new-val)
                    (mod-global-env-set! val new-val env)))]
            [recursively-extended-env-record (procnames idss bodiess old-env)
              (let loop ([count 0])
                  (if (>= count (length idss))
                      (mod-global-env-set! val new-val old-env)
                      (let ([pos (list-find-position val (list-ref idss count))])
                          (if (number? pos)
                              (list-set! vals pos new-val)
                              (loop (+ count 1))))))]
            [else (display 'mr-poopy-butt-hole)])))
; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

; (define get-ref-ids
;   (lambda (datum)
;     (map (lambda (x) (if (list? x)  ; map the cadr over the arguments list passed to get the x in (ref x)
;                         (cadr x)
;                         x))  ;; will it ever actually get to this?
;     datum)))



(define get-ref-count
  (lambda (datum)
    (let loop ([datum datum]
                [result '()]
                [count 0])
      (cond 
        [(null? datum) result]  ; if no more refs
        [(list? (car datum)) ; if ref
          (loop (cdr datum) (append result (list count)) (+ count 1))]  ; will keep track of the refs place
        [else (loop (cdr datum) result (+ count 1))]))))  ; if a regular item

(define parse-exp
  (lambda (datum)
    (cond
      [(symbol? datum)
        (var-exp datum)]

      [(and (list? datum) (not (null? datum)))
          (cond
            [(eqv? (1st datum) 'case)
              (case-exp (parse-exp (2nd datum)) (map (lambda (x) (lit-exp (car x))) (cddr datum))
                                                (map (lambda (x) (parse-exp (cadr x))) (cddr datum)))]
            [(eqv? (1st datum) 'cond)
              (cond-exp (map (lambda (x) (parse-exp (car x))) (cdr datum))
                        (map (lambda (x) (parse-exp (cadr x))) (cdr datum)))]
            [(eqv? (1st datum) 'while)
              (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
            [(eqv? (1st datum) 'and)
              (and-exp (map parse-exp (cdr datum)))]
            [(eqv? (1st datum) 'or)
              (or-exp (map parse-exp (cdr datum)))]
            [(eqv? (1st datum) 'begin)
              (begin-exp (map parse-exp (cdr datum)))]
            [(eqv? (1st datum) 'lambda)  ; test for the lambda cases
              (begin 
                ; (display (caar (2nd datum)))
              (cond
                [(null? (cdr datum))
                  (eopl:error 'parse-exp "Invalid lambda syntax: No parameter list or body:~s" datum)]
                [(null? (cddr datum))
                  (eopl:error 'parse-exp "Error in lambda expression: No body: ~s" datum)]
                [(symbol? (2nd datum))
                  (inf-arg-lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
                [(and (pair? (2nd datum)) (not (list? (2nd datum))))
                  (pair-arg-lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
                [(andmap (lambda (x) (or (symbol? x) (list? x))) (2nd datum))  ; the test for refs
                  (begin 
                    ; (display "grass tastes bad")
                  (ref-lambda-exp (map (lambda (x) 
                                          (if (list? x) (cadr x) x)) ; mapping each of the ids
                                  (2nd datum)) 
                          (get-ref-count (2nd datum)) (map parse-exp (cddr datum))))
                  ; (lambda-exp (2nd datum) (map parse-exp (cddr datum)))
                  ]
                [(and (list? (2nd datum)) (not (andmap symbol? (2nd datum))))
                  (eopl:error 'parse-exp "Invalid lambda syntax: Invalid parameter syntax:~s" datum)]
                
                [(not (or (symbol? (2nd datum)) (pair? (2nd datum)) (null? (2nd datum))))
                  (eopl:error 'parse-exp "Invalid lambda syntax: Invalid parameter list:~s" datum)]
                [else
                  (lambda-exp (2nd datum) (map parse-exp (cddr datum)))]))]              
            [(eqv? (1st datum) 'case-lambda)
              (case-lambda-exp (map car (cdr datum)) (map cdr (cdr datum)))]  ;**  send first the arguments, then the bodies of each case-lambda

            [(eqv? (1st datum) 'if)  ; test for the if cases
              (cond
                [(null? (cdr datum))
                  (eopl:error 'parse-exp "Invalid if syntax: No condition or results:~s" datum)]
                [(null? (cddr datum))
                  (eopl:error 'parse-exp "Invalid if syntax: No result" datum)]
                [(and (not (null? (cdddr datum))) (not (null? (cddddr datum))))
                  (eopl:error 'parse-exp "Invalid if syntax: Too many arguments:~s" datum)]
                [(not (or (= (length datum) 3) (= (length datum) 4)))
                  (eopl:error 'parse-exp "Invalid if syntax: incorrect arguments to if: ~s" datum)]
                [(= (length datum) 3)
                  (if-one-exp (parse-exp (2nd datum)) (parse-exp (3rd datum))  )]
                [else
                  (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))])]

            [(eqv? (1st datum) 'set!)  ; test for the set!
              (cond
                [(null? (cdr datum))
                  (eopl:error 'parse-exp "Invalid set! syntax: No variable to change or value:~s" datum)]
                [(not (symbol? (2nd datum)))
                  (eopl:error 'parse-exp "Invalid set! syntax: Variable to change not a symbol:~s" datum)]
                [(null? (cddr datum))
                  (eopl:error 'parse-exp "Invalid set! syntax: No value specified:~s" datum)]
                [(not (= (length datum) 3))
                  (eopl:error 'parse-exp "Invalid set! syntax: incorrect number of arguments to set!:~s" datum)]
                [else
                  (set!-exp (2nd datum) (parse-exp (3rd datum)))])]

            [(eqv? (1st datum) 'let*)  ; test for let*
            (cond
              [(null? (cdr datum))
                (eopl:error 'parse-exp "Invalid let* syntax: Invalid value list: " datum)]
              [(null? (cddr datum))
                (eopl:error 'parse-exp "Invalid let* syntax: No body: " datum)]
              [(not (list? (2nd datum)))
                (eopl:error 'parse-exp "Invalid let* syntax: Invalid variable/value list: " datum)]
              [(not (andmap (lambda (x) 
                              (and (list? x) (symbol? (1st x)) (eq? (length x) 2))) 
                    (2nd datum)))
                (eopl:error 'parse-exp "Invalid let* syntax: Invalid variable/value list: " datum)]
              [else
                (let*-exp (map car (2nd datum)) (map (lambda (x)
                                                              (parse-exp (2nd x))) (2nd datum)) (map parse-exp (cddr datum)))])]

            [(eqv? (1st datum) 'let)  ; test for let
                (if (symbol? (2nd datum) )
                    (cond ; named let 
                      [(< (length datum) 4)
                          (eopl:error 'parse-exp "Invalid let syntax: incorrect length: ~s" datum)]
                      [(not (list? (3rd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: not a list: ~s" datum)]
                      [(not (andmap list? (3rd datum))) 
                          (eopl:error 'parse-exp "Invalid let syntax: decl not all proper lists: ~s" datum)]
                      [(not (andmap (lambda (x)
                                        (= (length x) 2))
                              (3rd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: decls not all length 2: ~s" datum)]
                      [(not (andmap (lambda (x)
                                        (symbol? (1st x)))
                              (3rd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: decls first members must be symbols: ~s" datum)]
                      [else
                        (named-let-exp  (2nd datum)
                                        (map car (3rd datum)) 
                                        (map (lambda (x) (parse-exp (2nd x))) (3rd datum)) 
                                        (map parse-exp (cdddr datum)))])

                    (cond ; unnamed let 
                      [(< (length datum) 3)
                          (eopl:error 'parse-exp "Invalid let syntax: incorrect length: ~s" datum)]
                      [(not (list? (2nd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: not a list: ~s" datum)]
                      [(not (andmap list? (2nd datum))) 
                          (eopl:error 'parse-exp "Invalid let syntax: decl not all proper lists: ~s" datum)]
                      [(not (andmap (lambda (x)
                                        (= (length x) 2))
                              (2nd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: decls not all length 2: ~s" datum)]
                      [(not (andmap (lambda (x)
                                        (symbol? (1st x)))
                              (2nd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: decls first members must be symbols: ~s" datum)]
                      [else
                        (let-exp (map car (2nd datum)) (map (lambda (x)
                                                              (parse-exp (2nd x))) (2nd datum)) (map parse-exp (cddr datum)))]))]
            [(eqv? (1st datum) 'letrec)
              (cond ; letrec
                      [(> 3 (length datum))
                          (eopl:error 'parse-exp "Invalid let syntax: incorrect length: ~s" datum)]
                      [(not (list? (2nd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: not a list: ~s" datum)]
                      [(not (andmap list? (2nd datum))) 
                          (eopl:error 'parse-exp "Invalid let syntax: decl not all proper lists: ~s" datum)]
                      [(not (andmap (lambda (x)
                                        (= (length x) 2))
                              (2nd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: decls not all length 2: ~s" datum)]
                      [(not (andmap (lambda (x)
                                        (symbol? (1st x)))
                              (2nd datum)))
                          (eopl:error 'parse-exp "Invalid let syntax: decls first members must be symbols: ~s" datum)]

                      [else
                        (letrec-exp (map car (2nd datum)) ; names
                                    (map (lambda (x) (cadr (2nd x))) (2nd datum)) ; vars
                                    (map (lambda (x) (map parse-exp (cddr (cadr x)))) (2nd datum)) ; bodies
                                    (map parse-exp (cddr datum)))]  ; letrec body
              )] 
            [(eqv? (1st datum) 'quote)
              (lit-exp (cadr datum))]

            [(eqv? (1st datum) 'define)   ; define val exp
              (define-exp (2nd datum) (parse-exp (3rd datum)))]
            [else
               (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))])]     

      [(or (number? datum) (string? datum) (boolean? datum) (vector? datum) (null? datum) (list? datum))
        (lit-exp datum)]
      [(pair? datum)
          (eopl:error 'parse-exp "Error in parse-exp: Not a proper list: ~s" datum)]
      [else
        (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))
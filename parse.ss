; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (trace-lambda calling? (datum)
    (cond
      [(symbol? datum)
        (var-exp datum)]
      [(and (list? datum) (not (null? datum)))
          (cond
            [(eqv? (1st datum) 'lambda)  ; test for the lambda cases
              (cond
                [(null? (cdr datum))
                  (eopl:error 'parse-exp "Invalid lambda syntax: No parameter list or body:~s" datum)]
                [(null? (cddr datum))
                  (eopl:error 'parse-exp "Error in lambda expression: No body: ~s" datum)]
                [(and (list? (2nd datum)) (not (andmap symbol? (2nd datum))))
                  (eopl:error 'parse-exp "Invalid lambda syntax: Invalid parameter syntax:~s" datum)]
                [(not (or (symbol? (2nd datum)) (pair? (2nd datum)) (null? (2nd datum))))
                  (eopl:error 'parse-exp "Invalid lambda syntax: Invalid parameter list:~s" datum)]
                [else
                  (lambda-exp (2nd datum) (map parse-exp (cddr datum)))])]

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
                [else
                  (if-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]

            [(eqv? (1st datum) 'set!)  ; test for the set!
              (cond
                [(null? (cdr datum))
                  (eopl:error 'parse-exp "Invalid set! syntax: No variable to change or value:~s" datum)]
                [(not (symbol? (2nd datum)))
                  (eopl:error 'parse-exp "Invalid set! syntax: Variable to change not a symbol:~s" datum)]
                [(null? (cddr datum))
                  (eopl:error 'parse-exp "Invalid set! syntax: No value specified:~s" datum)]
                ; [(not (null? (cdddr datum)))
                ;   (eopl:error 'parse-exp "Invalid set! syntax ~s\n~s" datum "Too many arguments")]
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
                  (let*-exp (map (lambda (x) 
                                              (list (1st x) (parse-exp (2nd x)))) (2nd datum)) (map parse-exp (cddr datum)))])]


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
                          (named-let-exp (2nd datum) (map (lambda (x) 
                                              (list (1st x) (parse-exp (2nd x)))) (3rd datum)) (map parse-exp (cdddr datum)))])

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
                          (let-exp (map (lambda (x) 
                                              (list (1st x) (parse-exp (2nd x)))) (2nd datum)) (map parse-exp (cddr datum)))]))]


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
                          (letrec-exp (map (lambda (x) 
                                              (list (1st x) (parse-exp (2nd x)))) (2nd datum)) (map parse-exp (cddr datum)))])]


                        
            [else
              (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))])]
      [(pair? datum)
          (eopl:error 'parse-exp "Error in parse-exp: Not a proper list: ~s" datum)]
      [(or (number? datum) (string? datum) (boolean? datum) (vector? datum) (null? datum) (list? datum))
        (lit-exp datum)]
      [else
        (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))












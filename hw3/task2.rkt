#lang plai

#| Task2. Implement RLFAE that supports a recursion based on the following requirements. |#

#| BNF
<RLFAE> :: = <num>
             | {+ <RLFAE> <RLFAE>}
             | {- <RLFAE> <RLFAE>}
             | {* <RLFAE> <RLFAE>}
             | {= <RLFAE> <RLFAE>}
             | <id>
             | {fun {<id>} <RLFAE>}
             | {ifexp <RLFAE> <RLFAE> <RLFAE>}
             | {orop <RLFAE> <RLFAE>}
             | {rec {<id> <RLFAE>} <RLFAE>}
|#

;why using same BNF??
;--------------------------------------------------------------------------------------
(define-type RLFAE
    [num (n number?)]
    [add (lhs RLFAE?) (rhs RLFAE?)]
    [sub (lhs RLFAE?) (rhs RLFAE?)]
    [mul (lhs RLFAE?) (rhs RLFAE?)]
    [same (lhs RLFAE?) (rhs RLFAE?)]
    [id  (name symbol?)]
    [fun (param symbol?) (body RLFAE?)]
    [app (fun-expr RLFAE?) (arg-expr RLFAE?)]
    [ifexp (test-expr RLFAE?)
           (then-expr RLFAE?) (else-expr RLFAE?)]
    [orop (lhs RLFAE?) (rhs RLFAE?)]
    [rec (name symbol?) (named-expr RLFAE?) (fst-call RLFAE?)])
;--------------------------------------------------------------------------------------
;[contract] : sexp => RLFAE
;[purpose] : to convert s-expressions into RLFAEs in abstract syntax
;[tests] : (test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
;          (test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
(define (parse sexp)
   (match sexp
      [(? number?)                       (num sexp)]
      [(list '+ l r)                     (add (parse l) (parse r))]
      [(list '- l r)                     (sub (parse l) (parse r))]
      [(list '* l r)                     (mul (parse l) (parse r))]
      [(list 'with (list i v) e)         (app (fun i (parse e)) (parse v))]
      [(? symbol?)                       (id sexp)]
      [(list 'fun (list p) b)            (fun p (parse b))]
      [(list f a)                        (app (parse f) (parse a))]
      [(list 'ifexp te th el)            (ifexp (parse te) (parse th)  (parse el))]
      [(list 'orop l r)                  (orop (parse l) (parse r))]
      [(list 'rec (list rfn ne) body)    (rec rfn (parse ne) (parse body))]
      [else                              (error 'parse "bad syntax: ~a" sexp)]))
;--------------------------------------------------------------------------------------
(define-type DefrdSub
    [mtSub]
    [aSub  (name symbol?) (value RLFAE-Value?) (ds DefrdSub?)]
    [aRecSub  (name symbol?) (value-box (box/c RLFAE-Value?)) (ds DefrdSub?)])
;--------------------------------------------------------------------------------------
(define-type RLFAE-Value
    [numV        (n number?)]
    [closureV    (param symbol?) (body RLFAE?) (ds DefrdSub?)]
    [exprV       (expr RLFAE?) (ds DefrdSub?) (value (box/c (or/c false RLFAE-Value?)))])
;--------------------------------------------------------------------------------------
;[contract] : RLFAE-Value -> RLFAE-Value
;[purpose] : save value in the box to be possible to reuse
;[tests] : (test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
;          (test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
(define (strict v)
    (type-case RLFAE-Value v
        [exprV (expr ds v-box)
                     (if (not (unbox v-box))
                          (local [(define v (strict (interp expr ds)))]
                              (begin (set-box! v-box v)
                                           v))
                          (unbox v-box))] 
        [else v]))
;--------------------------------------------------------------------------------------
;[contract] : num-op -> lambda
;[purpose] : to causes more function calls, using num-op
;[tests] : (test (num+ (numV 3) (numV 6)) (numV 9))
;          (test (num- (numV 2) (numV 1)) (numV 1))
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n (strict x)) (numV-n (strict y))))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))
(define num= (num-op =))
;--------------------------------------------------------------------------------------
;[contract] : name => LFAE-Value
;[purpose] : find value and save or print error
;[tests] : (test (lookup 'x (aSub 'x (numV 1) (mtSub))) (numV 1))
;        : (test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 4) (mtSub)))) (numV 4))
(define (lookup name ds)
    (type-case DefrdSub ds
        [mtSub  () (error 'lookup "free variable")]
        [aSub    (sub-name val rest-ds)
                                  (if (symbol=? sub-name name)
                                        val
                                        (lookup name rest-ds))]
        [aRecSub (sub-name val-box rest-ds)
                          (if (symbol=? sub-name name)
                               (unbox val-box)
                               (lookup name rest-ds))]))

;--------------------------------------------------------------------------------------
;[contract] : RLFAE-Value -> boolean
;[purpose] : check the num is zero or not
;[tests] : (test (numzero? (numV 0)) #t)
;        : (test (numzero? (numV 1)) #f)
(define (numzero? n)
    (zero? (numV-n n)))
;--------------------------------------------------------------------------------------
;[contract] : RLFAE-Value -> boolean
;[purpose] : checks if an RLFAE expression contains recursion or not
;[tests] : (test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
;          (test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
;(define (is-recursion exp)
;  (~))
;--------------------------------------------------------------------------------------
;[contract] : RLFAE-Value -> boolean
;[purpose] : converts the RLFAE expression containing recursion into another expression avoiding the free identifier issue from the recursion.
;[tests] : (test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
;          (test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
;(define (desugar exp)
;  (~))
;--------------------------------------------------------------------------------------
;[contract] : RLFAE-DefrdSub -> RLFAE-Value
;[purpose] : takes a program and produces a result
;[tests] : (test (interp (parse '{rec {count {fun {n} {ifexp n 0 {+ 1 {count {- n 1}}}}}} {count 8}} ) (mtSub) ) (numV 8) )
;          (test (interp (parse '{rec {fac {fun {n} {ifexp n 1 {* n {fac {- n 1}}}}}}{fac 10}} ) (mtSub) ) (numV 3628800) )
(define (interp rlfae ds)
    (type-case RLFAE rlfae
        [num  (n)         (numV n)]
        [add  (l r)       (num+ (interp l ds) (interp r ds))]
        [sub  (l r)       (num- (interp l ds) (interp r ds))]
        [mul  (l r)       (num* (interp l ds) (interp r ds))]
        [same (l r)       (num= (interp l ds) (interp r ds))]
        [id   (s)         (strict (lookup s ds))]
        [fun  (p b)       (closureV p b ds)]
        [app  (f a)       (local [(define f-val (strict (interp f ds)))
                                  (define a-val (exprV a ds (box #f)))]
                                     (interp (closureV-body f-val)
                                             (aSub (closureV-param f-val)
                                             a-val
                                              (closureV-ds f-val))))]
        [ifexp  (test-expr then-expr else-expr)
                          (if (numzero? (interp test-expr ds))
                                    (interp then-expr ds)
                                    (interp else-expr ds))]
        [orop (l r)       (orop (interp l ds) (interp r ds))]
        [rec  (bound-id named-expr first-call)
                 (local [(define value-holder (box (numV 1234)))
                             (define new-ds (aRecSub bound-id
                                                                           value-holder   ds))]
                            (begin
                                 (set-box! value-holder (interp named-expr new-ds))
                                 (interp first-call new-ds)))]))
;--------------------------------------------------------------------------------------
;[contract] : sexp ds -> interp
;[purpose] : convert sexp ds to interp to run
;[tests] : (test (run '{rec {count {fun {n} {ifexp n 0 {+ 1 {count {- n 1}}}}}} {count 8}} (mtSub) ) (numV 8) )
;          (test (run '{rec {fac {fun {n} {ifexp n 1 {* n {fac {- n 1}}}}}}{fac 10}} (mtSub) ) (numV 3628800) )
(define (run sexp ds)
       (interp (parse sexp) ds))

#|
(define (run sexp)
     (if (equal? (is-recursion sexp) true)
         (interp (parse (desugar sexp)) (mtSub))
         (interp (parse sexp) (mtSub))))|#


;------test--------
(test (run '{rec {count {fun {n} {ifexp n 0 {+ 1 {count {- n 1}}}}}} {count 8}} (mtSub) ) (numV 8) )
(test (run '{rec {fac {fun {n} {ifexp n 1 {* n {fac {- n 1}}}}}}{fac 10}} (mtSub)) (numV 3628800))
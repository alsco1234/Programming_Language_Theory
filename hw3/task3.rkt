#lang plai

#| Task3. Implement RCFAEDS that supports a resursion based on the following requirements.|#

(define-type RCFAE
    [num (n number?)]
    [add (lhs RCFAE?) (rhs RCFAE?)]
    [sub (lhs RCFAE?) (rhs RCFAE?)]
    [mul (lhs RCFAE?) (rhs RCFAE?)]
    [same (lhs RCFAE?) (rhs RCFAE?)]
    [id  (name symbol?)]
    [fun (param symbol?) (body RCFAE?)]
    [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
    [ifexp (test-expr RCFAE?)
           (then-expr RCFAE?) (else-expr RCFAE?)]
    [orop (lhs RCFAE?) (rhs RCFAE?)]
    [with(name symbol?) (name-expr RCFAE?) (body RCFAE?)])


; parse : secp => LFAE
(define (parse sexp)
   (match sexp
      [(? number?)                (num sexp)]
      [(list '+ l r)              (add (parse l) (parse r))]
      [(list '- l r)              (sub (parse l) (parse r))]
      [(list '* l r)              (mul (parse l) (parse r))]
      [(list '= l r)              (same (parse l) (parse r))]
      [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
      [(? symbol?)                (id sexp)]
      [(list 'fun (list p) b)                 (fun p (parse b))]
      [(list f a)                 (app (parse f) (parse a))]
      [(list 'ifexp te th el)            (ifexp (parse te) (parse th)  (parse el))]
      [(list 'orop l r)                  (orop (parse l) (parse r))]
      [(list with (list rfn ne) body)    (with rfn (parse ne) (parse body))]
      [else                       (error 'parse "bad syntax: ~a" sexp)]))


(define-type DefrdSub
    [mtSub]
    [aSub  (name symbol?) (value RCFAE-Value?) (ds DefrdSub?)]
    [aRecSub  (name symbol?) (value-box (box/c RCFAE-Value?)) (ds DefrdSub?)])


(define-type RCFAE-Value
    [numV        (n number?)]
    [closureV    (param symbol?) (body RCFAE?) (ds DefrdSub?)]
    [exprV       (expr RCFAE?) (ds DefrdSub?) (value (box/c (or/c false RCFAE-Value?)))])


; strict : LFAE-Value -> LFAE-Value
(define (strict v)
    (type-case RCFAE-Value v
        [exprV (expr ds v-box)
                     (if (not (unbox v-box))
                          (local [(define v (strict (interp expr ds)))]
                              (begin (set-box! v-box v)
                                           v))
                          (unbox v-box))] 
        [else v]))


(define (num-op op)
     (lambda (x y)
          (numV (op (numV (strict x)) (numV (strict y))))))


(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))
(define num= (num-op =))


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


; numzero? : RCFAE-Value -> boolean
(define (istrue? sexp)
  (match sexp
      [(list '= l r)              (issame? (parse l) (parse r))]
      [else                       (error 'parse "bad syntax: ~a" sexp)]))

(define (issame? r l)
    (zero? (num- (numV-n r) (numV-n l)) ))


(define (interp rcfae ds)
    (type-case RCFAE rcfae
        [num  (n)         (numV n)]
        [add  (l r)       (num+ (interp l ds) (interp r ds))]
        [sub  (l r)       (num- (interp l ds) (interp r ds))]
        [mul  (l r)       (num* (interp l ds) (interp r ds))]
        [same (l r)       (issame? (interp l ds) (interp r ds))]
        [id   (s)         (strict (lookup s ds))]
        [fun  (p b)       (closureV p b ds)]
        [app  (f a)       (local [(define f-val (strict (interp f ds)))
                                  (define a-val (exprV a ds (box #f)))]
                                     (interp (closureV-body f-val)
                                             (aSub (closureV-param f-val)
                                             a-val
                                              (closureV-ds f-val))))]
        [ifexp  (test-expr then-expr else-expr)
                          (if (istrue? (interp test-expr ds))
                                    (interp then-expr ds)
                                    (interp else-expr ds))]
        [orop (l r)       (orop (interp l ds) (interp r ds))]

        [with  (bound-id named-expr first-call)
                 (local [(define value-holder (box (numV 1234)))
                             (define new-ds (aRecSub bound-id
                                                                           value-holder   ds))]
                            (begin
                                 (set-box! value-holder (interp named-expr new-ds))
                                 (interp first-call new-ds)))]))

(define (run sexp ds)
     (interp (parse sexp) ds))
;------test--------
;{with {count {fun {n} {ifs {= n 0} 0 {+ 1 {count {- n 1}}}}}} {count 8}} ; Output: (numV 8)
;{with {fac {fun {n}{if {= n 0} 1 {* n {fac {- n 1}}}}}}{fac 10}} ; Output: (numV 3628800)
;{with {fib {fun {n} {if {or {= n 0} {= n 1}} 1 {+ {fib {- n 1}} {fib {- n 2}}}}}} {fib 10}} ; Output: (numV 89)
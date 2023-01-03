#lang plai

(define-type RCFAE
    [num (n number?)]
    [add (lhs RCFAE?) (rhs RCFAE?)]
    [sub (lhs RCFAE?) (rhs RCFAE?)]
    [mul (lhs RCFAE?) (rhs RCFAE?)]
    [id (name symbol?)]
    [fun (param symbol?) (body RCFAE?)]
    [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
    [if0 (test-expr RCFAE?)
           (then-expr RCFAE?) (else-expr RCFAE?)]
    [rec (name symbol?) (named-expr RCFAE?) (body RCFAE?)])

(define (parse sexp)
   (match sexp
      [(? number?)                (num sexp)]
      [(list '+ l r)              (add (parse l) (parse r))]
      [(list '- l r)              (sub (parse l) (parse r))]
      [(list '* l r)              (mul (parse l) (parse r))]
      [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
      [(? symbol?)                (id sexp)]
      [(list 'fun (list p) b)                 (fun p (parse b))]
      [(list f a)                 (app (parse f) (parse a))]
      [(list 'if0 te th el)       (if0 (parse te) (parse th)  (parse el))]
      [(list 'rec (list rfn ne) body)    (rec rfn (parse ne) (parse body))]
      [else                       (error 'parse "bad syntax: ~a" sexp)]))

(define-type DefrdSub
  [mtSub]
  [aSub    (name symbol?) (value RCFAE-Value?) (ds DefrdSub?)]
  [aRecSub (name symbol?)
           (value-box (box/c RCFAE-Value?))
           (ds DefrdSub?)])

(define-type RCFAE-Value
  [numV       (n number?)]
  [closureV   (param symbol?) (body RCFAE?) (ds DefrdSub?)]
  [exprV      (expr RCFAE?) (ds DefrdSub?)
                            (value (box/c (or/c false RCFAE-Value?)))])

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
          (numV (op (numV-n (strict x)) (numV-n (strict y))))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))


(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (id v saved) (if(symbol=? id name)
                                v
                                (lookup name saved))]
    [aRecSub (id val-box rest-ds)
             (if (symbol=? id name)
                 (unbox val-box)
                 (lookup name rest-ds))]))

(define (numzero? n)
 (zero? (numV-n n)))

(define (interp lfae ds)
  (type-case RCFAE lfae
     [num (n)      (numV n)]
     [add (l r)    (num+ (interp l ds) (interp r ds))]
     [sub (l r)    (num- (interp l ds) (interp r ds))]
     [mul (l r)    (num* (interp l ds) (interp r ds))]
     [id  (s)      (strict (lookup s ds))]
     [fun (p b)    (closureV p b ds)]
     [if0 (test-expr then-expr else-expr)
                   (if(numzero? (interp test-expr ds))
                           (interp then-expr ds)
                           (interp else-expr ds))]
     [app (f a)   (local [(define f-val (strict (interp f ds)))
                          (define a-val (exprV a ds (box #f)))]
                   (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val))))]
     [rec (bound-id named-expr first-call)
                       (local [(define value-holder (box (numV 198)))
                               (define new-ds (aRecSub bound-id
                                                       value-holder
                                                       ds))]
                              (begin
                                (set-box! value-holder (interp named-expr new-ds))
                                (interp first-call new-ds)))]))


(define (run sexp ds)
     (interp (parse sexp) ds))

(run '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}}
                  {count 8}} (mtSub))

(run '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count {count 8}}} (mtSub))

(run '{rec {fac {fun {n} {if0 n 1 {* n {fac {- n 1}}}}}}
                  {fac 10}} (mtSub))

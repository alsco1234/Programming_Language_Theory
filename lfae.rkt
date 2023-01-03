#lang plai

(define-type LFAE
    [num    (n number?)]
    [add     (lhs LFAE?) (rhs LFAE?)]
    [sub     (lhs LFAE?) (rhs LFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body LFAE?)]
    [app     (ftn LFAE?) (arg LFAE?)])
  
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])

(define-type LFAE-Value
  [numV       (n number?)]
  [closureV   (param symbol?) (body LFAE?) (ds DefrdSub?)]
  [exprV      (expr LFAE?) (ds DefrdSub?)
                            (value (box/c (or/c false LFAE-Value?)))]) ; the thrid field is for cache for the value of expression after one-time evaluation.


(define (strict v)
    (type-case LFAE-Value v
        [exprV (expr ds v-box)
                     (if (not (unbox v-box))
                          (local [(define v (strict (interp expr ds)))]
                              (begin (set-box! v-box v)
                                           v))
                          (unbox v-box))] 
        [else v]))

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))


(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i v saved) (if(symbol=? i name)
                                (strict v)             ;; if v is exprV (num ==> interp it
                                (lookup name saved))]))

(define (interp lfae ds)
  (type-case LFAE lfae
     [num (n)      (numV n)]
     [add (l r)    (num+ (interp l ds) (interp r ds))]
     [sub (l r)    (num- (interp l ds) (interp r ds))]
     [id  (s)     (lookup s ds)]
     [fun (p b)  (closureV p b ds)]
     [app (f a)   (local [(define f-val (strict (interp f ds)))
                          (define a-val (exprV a ds (box #f)))]
                   (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val))))]))

(define (run sexp ds)
     (interp (parse sexp) ds))


(run '{{fun {f} {f 1}} {fun {x} {+ x 1}}} (mtSub))
(run '{with {a {fun {f} {+ f f}}} {a 3}} (mtSub))


 
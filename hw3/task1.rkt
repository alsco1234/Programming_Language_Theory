#lang plai

#| Task1. Implement LFAE Language by using Racket |#

;--------------------------------------------------------------------------------------
(define-type LFAE
    [num    (n number?)]
    [add     (lhs LFAE?) (rhs LFAE?)]
    [sub     (lhs LFAE?) (rhs LFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body LFAE?)]
    [app     (ftn LFAE?) (arg LFAE?)])
;--------------------------------------------------------------------------------------
;[contract] : secp => LFAE
;[purpose] : to convert s-expressions into LFAEs in abstract syntax
;[tests] : (test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
;          (test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")
(define (parse sexp)
   (match sexp
        [(? number?)             (num sexp)]
        [(list '+ l r)                  (add (parse l) (parse r))]
        [(list '- l r)                   (sub (parse l) (parse r))]
        [(list 'with (list i v) e)   (app i(parse v) (parse e))]
        [(? symbol?)             (id sexp)]
        [(list 'fun (list p) b)    (fun p (parse b))]
        [(list f a)                   (app (parse f) (parse a))]
        [else                        (error 'parse "bad syntax: ~a" sexp)]))
;--------------------------------------------------------------------------------------
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])
;--------------------------------------------------------------------------------------
(define-type LFAE-Value
  [numV       (n number?)]
  [closureV   (param symbol?) (body LFAE?) (ds DefrdSub?)]
  [exprV      (expr LFAE?) (ds DefrdSub?)
                            (value (box/c (or/c false LFAE-Value?)))])
;--------------------------------------------------------------------------------------
;[contract] : LFAE-Value => LFAE-Value
;[purpose] : save value in the box to be possible to reuse
;[tests] : (test (strict (closureV 'x (add (id 'y) (id 'x)) (aSub 'y (numV 10) (mtSub)))) (closureV 'x (add (id 'y) (id 'x)) (aSub 'y (numV 10) (mtSub))))
;        : (test (strict (numV 7)) (numV 7))
(define (strict e)
   (type-case LFAE-Value e
     [exprV (expr ds v-box)
            (if (not (unbox v-box))
                (local [(define v (strict (interp expr ds)))]
                  (begin (set-box! v-box v)
                               v))
                (begin
                  (unbox v-box)))]
     [else e]))
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
;--------------------------------------------------------------------------------------
;[contract] : name => LFAE-Value
;[purpose] : find value and save or print error
;[tests] : (test (lookup 'x (aSub 'x (numV 1) (mtSub))) (numV 1))
;        : (test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 4) (mtSub)))) (numV 4))
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i v saved) (if(symbol=? i name)
                                v       
                                (lookup name saved))]))
;--------------------------------------------------------------------------------------
;[contract] : LFAE DefrdSub -> LFAE-Value
;[purpose] : takes a program and produces a result
;[tests] : (test (interp (parse '{{fun {x} 0} {+ 1 {fun {y} 2}}}) (mtSub)) (numV 0))
;        : (test (interp (parse '{{fun {x} x} {+ 1 {fun {y} 2}}}) (mtSub))  ==> ERROR!)
(define (interp lfae ds)
  (type-case LFAE lfae
     [num (n)     (numV n)]
     [add (l r)   (num+ (interp l ds) (interp r ds))]
     [sub (l r)   (num- (interp l ds) (interp r ds))]
     [id  (s)     (strict (lookup s ds))]
     [fun (p b)   (closureV p b ds)]
     [app (f a)   (local [(define f-val (strict (interp f ds)))
                                 (define a-val (exprV a ds (box #f)))]
                              (interp (closureV-body f-val)
                                         (aSub (closureV-param f-val)
                                          a-val
                                          (closureV-ds f-val))))]))
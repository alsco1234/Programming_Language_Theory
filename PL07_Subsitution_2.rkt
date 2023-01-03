#lang plai

#| Subsitution (2) |#

; 1. Define type WAE!
(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
)

#| ************************************************************************************* |#

; 2. Implemet a parser for WAE! (by usung match)
(define (parse sexp)
  (match sexp
    [(? number?)                (num sexp)]
    [(list ' + l r)             (add (parse l) (parse r))]
    [(list ' - l r)             (sub (parse l) (parse r))]
    [(list 'with (list i v)e)   (with i (parse v) (parse e))] ;with
    [(? symbol?)                (id sexp)] ;is symbo?
    [else (error 'parse "bad syntax:~a" sexp)]
))

(test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
(test (parse '{with {x 5} {+ 8 2}}) (with 'x (num 5) (add (num 8) (num 2))))
(test (parse '{with {x 5} {+ x x}}) (with 'x (num 5) (add (id 'x) (id 'x))))

#| ************************************************************************************* |#

; 3. Implemetn subsitution for WAE interpreter
; [contract] subst: WAE symbol number -> WAE
(define (subst wae idtf val)
	(type-case WAE wae
		[num	(n)		wae]
		[add	        (l r) 		(add (subst l idtf val) (subst r idtf val))]
		[sub		(l r)	 	(sub (subst l idtf val) (subst r idtf val))]
		[with	(i v e) 	(with i (subst v idtf val) (if (symbol=? i idtf) e
									(subst e idtf val)))]
		[id		(s) 		(if (symbol=? s idtf) (num val) wae)]
))

;[tests]
;  
;{with {x 10} 5}                        => 10 for x in 5               => 5
(test (subst (num 5) 'x 10) (num 5))
;
;{with {x 10} 5}                        => 10 for x in 5 {+ 1 x}       => {+ 1 10}
(test (subst (add (num 1) (id 'x)) 'x 10) (add (num 1) (num 10)))
;
;{with {x 10} x}                        => 10 for x in x               => 10
(test (subst (id 'x) 'x 10) (num 10))
;
;{with {x 10} y}                        => 10 for x in y               => y (no subsitution)
(test (subst (id 'y) 'x 10) (id 'y))
;
;{with {y 10} {- x 1}}                  => 10 for y in {- x 1}(no subsitution)
(test (subst (sub (id 'x) (num 1)) 'y 10)    (sub (id 'x) (num 1)))
;
;{with {x 10} {...{with {y 17} x}}      => 10 for x in {with {y 17} x} 	=>  {with {y 17} 10}
(test (subst (with 'y (num 17) (id 'x)) 'x 10) (with 'y (num 17) (num 10)))
;
;{with {x 10} {...{with {y x} y}}}	=> 10 for x in {with {y x} y} 	=>  {with {y 10} y} 
(test (subst (with 'y (id 'x) (id 'y)) 'x 10) (with 'y (num 10) (id 'y)))
;
;{with {x 10} {...{with {x y} x}}}	=> 10 for x in {with {x y} x} 	=> {with {x y} x}
(test (subst (with 'x (id 'y) (id 'x)) 'x 10) (with 'x (id 'y) (id 'x)))

#| ************************************************************************************* |#

; 4. Implement WAE interpreter
(define (interp wae)
	(type-case WAE wae
		[num (n) n]
		[add (l r) (+ (interp l) (interp r))]
		[sub (l r) (- (interp l) (interp r))]
		[with (i v e) (interp (subst e i (interp v)))]
		[id (s) (error 'interp "free identifier")]
))

(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) 10)
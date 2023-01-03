#lang plai

#| Subsitution |#
; - identifier : just name. can't be constant. can reuse. (ex function, variable)
; - variable : name. can be constant. can't reuse
;            : memory location that value never change.

; Repeated expressio
; (10!)*3? => identifier 10! and multipy 3

; {+ {- {+ 5 5} 3} {- {+ 5 5} 3}
; {with {x {+ 5 5}} {with {y {- x 3} {+ y y}}} }

; <WAE> ::= <num>
;        |{+ <WAE> <WAE>}
;        |{+ <WAE> <WAE>}
;        |{with {<id> <WAE>} <WAE>}
;        | <id>

; ...free identifier
;{with {x {+ 1 2}} { with {y {- 4 3}} {+ x x}} }
; identifier that not bounded(no use now)

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)]
)

#|
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (= 3 (length sexp))
          (eq? (first sexp) '+) )
     (add (parse (second sexp)) (parse (third sexp)))]  ;is +?
    [(and (= 3 (length sexp))
          (eq? (first sexp) '-) )
     (sub (parse (second sexp)) (parse (third sexp)))]  ;is +?
;    [ ...(with ...)]
;    [ ... (id ...)]
    [else (error 'parse "bad syntax: ~a" sexp)]
))|#

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




#| Defining Subsitution |#
; ** Definiton 1 ***
; i: x
; v: 5
; e: {+ x x}
; {with {x 5} {+ 5 5}}

; 1) free identifir => no subsitution
; {with {x 5} {+ 10 y}}  => no subsitution
; free identifier <-> bound identifier

; 2) alerady identifier => reject
; {with {x 5} {+ x {with {x 3} 10}}}  => no

; 3) free and bouded identifier can be differ with scope
; {with {x 5} {+ x {with {y 3} x}}}
;                              ^
; > for that x, in scope of y, x is free
; > but in scope of wider, x is bounded


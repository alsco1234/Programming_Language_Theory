#lang plai

#| 1. parser |#

;Example syntax of new arithmetic expressions(AE)
; {+ {- 3 4 } 7} = 6

; Specify in BNF
; <AE> ::= <num>
;        |{+ <AE> <AE>}
;        |{- <AE> <AE>}

; abstract syntax representation (tree) in Racket
(define-type AE
  [num (n number?)]
  [add (lhs AE?)       ;if num, no recursion
           (rhs AE?)]
  [sub (lhs AE?)
            (rhs AE?)]
)

; define
(define ae1 (add (sub (num 3) (num 4)) (num 7)))
(sub? ae1) ;#f. instance of AE
(add-lhs ae1) ;(sub (num 3) (num 4))
(add-rhs ae1) ;(num 7)

; parse : sexp(sub expression) -> AE
; to convert sexp into AEs in abstract syntax

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
    [else (error 'parse "bad syntax: ~a" sexp)]
))
|#
; = using match
(define (parse sexp)
  (match sexp
    [(? number?)                (num sexp)]
    [(list ' + l r)             (add (parse l) (parse r))]
    [(list ' - l r)             (sub (parse l) (parse r))]
    [else (error 'parse "bad syntax:~a" sexp)]
))

  
;test
(test (parse '3) (num 3))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{+ {- 5 4} 5}) (add (sub (num 5) (num 4)) (num 5)))
; (parse '{+ 3 4 5}) ;error "parse: bad syntax: (+ 3 4 5)

;                {+{+12}5}
; Parser      => (add (add (num 1) (num 2)) (num 5))
; Interpreter => 8



#| 2. Type Deconstruction for the AE interpreter |#
; interp: AE -> number
(define (interp an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))] ;to retuen actual num
    [sub (l r) (- (interp l) (interp r))] ;if (- l r), {+ 1 2} no work
   )
)

(interp (parse '3)) ;3
(interp (parse '{+ 1 2})) ;3
(interp (parse '{- {+ 1 2} 5})) ;-2


















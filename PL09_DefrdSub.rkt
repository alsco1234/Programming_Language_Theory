#lang plai
#| Deferring Subsitution |#

; Representing Deferred Substitution
; interp : WAE -> number
; interp : WAE DefrdSub -> number

(define-type DefrdSub
  [mtSub] ;empty cache (repository
  [aSub (name symbol?)
         (value number?)
         (saved DefrdSub?)]
)

(aSub 'x 1 (aSub 'y 4 (aSub 'x 2 (mtSub)))) ;last must be empty

#| ************************************************************************************* |#

; WAE Interpreter with DefrdSub

(define (interp wae ds)
	(type-case WAE wae
		[num (n) n]
		[add (l r ds) (+ (interp l) (interp r ds))]
		[sub (l r ds) (- (interp l) (interp r ds))] 
		[with (i v e) (interp (subst e i (interp v)))]
		[id (s) (lookup s ds)] ;s = id, ds = value
)) 
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          (lookup name saved))])
  )


; static vs dynamic ds or (mtSub) in the app branch
(test (interp (parse '{with {x 3} {f 1}}
                     (list (parse-fd '(deffyn (f y) {+ y x}))) (mtSub)) 4)
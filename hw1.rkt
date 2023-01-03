#lang plai

; Problem 1:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] dollar->won : number(int) -> number(int)
; [purpose] To convert doller to won
; [tests] (test (dollar->won 1) 1342)
;         (test (dollar->won 20) 26840)
;         (test (dollar->won 300) 402600)

(define (dollar->won dollar)
  (* 1342 dollar))

(test (dollar->won 1) 1342)
(test (dollar->won 20) 26840)
(test (dollar->won 300) 402600)


; Problem 2:
; Solved by myself: Y
; Time taken: about 3 mins
; [contract] digit_sum : 3 numbers(int) -> number(int)
; [purpose] To sum 3 numbers
; [tests] (test (digit_sum 1 2 3) 6)
;         (test (digit_sum 40 50 60) 150)
;         (test (digit_sum 700 800 900) 2400)

(define (digit_sum a b c)
  (+ a (+ b c)))

(test (digit_sum 1 2 3) 6)
(test (digit_sum 40 50 60) 150)
(test (digit_sum 700 800 900) 2400) 


; Problem 3:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] volume-sphere : number(int) -> number(float)
; [purpose] To calculate volume of the sphere
; [tests] (test (volume-sphere 1) 3.141592653589793)
;         (test (volume-sphere 20) 1256.6370614359173)
;         (test (volume-sphere 300) 282743.3388230814)

(define (volume-sphere r)
  (* pi (* r r)))

(test (volume-sphere 1) 3.141592653589793)
(test (volume-sphere 20) 1256.6370614359173)
(test (volume-sphere 300) 282743.3388230814)


; Problem 4:
; Solved by myself: Y
; Time taken: about 5 mins
; [contract] is-even : number(int) -> boolean
; [purpose] To retrun whether the number is even
; [tests] (test (is-even 1) #f)
;         (test (is-even 20) #t)
;         (test (is-even 300) #t)

(define (is-even n)
  (= 0 (modulo n 2)))

(test (is-even 1) #f)
(test (is-even 20) #t)
(test (is-even 300) #t)


; Problem 5:
; Solved by myself: Y
; Time taken: about 15 mins
; [contract] combination : 2 numbers(int) -> number(int)
; [purpose] To return the number of combinations
; [tests] (test (combination 0 0) 1)
;         (test (combination 0 1) 0)
;         (test (combination 1 0) 1)
;         (test (combination 3 2) 3)
;         (test (combination 2 3) 0)

(define (recursion n)
  (cond
    [(= 0 n) 1]
    [(= 1 n) 1]
    [else (* n (recursion (- n 1))) ] ))
(define (combination n k)
  (cond
    [(< n k) 0]
    [else (/ (recursion n) (* (recursion k) (recursion(- n k)))) ] ))

(test (combination 0 0) 1)
(test (combination 0 1) 0)
(test (combination 1 0) 1)
(test (combination 3 2) 3)
(test (combination 2 3) 0)


; Problem 6-a:
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] Vehicle : Bycycle(wheels), Car(wheels, windows), Airplane(wheels, windows, engines)
; [purpose] To Define the type Vehicle, which has three variants
; [tests] (define myA (Airplane 2 11 1))
;         (define myC (Car 1 2))
;         ;they are just define-type which has no result

(define-type Vehicle
  (Bycycle  (wheels number?))
  (Car      (wheels number?)
            (windows number?))
  (Airplane (wheels number?)
            (windows number?)
            (engines number?))
 )

(define myA (Airplane 2 11 1))
(define myC (Car 1 2))
;they are just define-type which has no result

; Problem 6-b:
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] vehicle-tax : Vehicle tax-per-wheel tax-per-window tax-per-engine -> number(int)
; [purpose] To calculate tax per wheel, window, engine and returns the total amount of tax
; [tests] (test (vehicle-tax myA 100 200 300) 2700)
;         (test (vehicle-tax myC 100 200 v300) 500)

(define (get-wheels v)
  (type-case Vehicle v
    [Bycycle (w) w]
    [Car (w wi) w]
    [Airplane (w wi e) w])
 )
(define (get-windows v)
  (type-case Vehicle v
    [Bycycle (w) 0]
    [Car (w wi) wi]
    [Airplane (w wi e) wi])
 )
(define (get-engines v)
  (type-case Vehicle v
    [Bycycle (w) 0]
    [Car (w wi) 0]
    [Airplane (w wi e) e])
 )
(define (vehicle-tax Vehicle tax-per-wheel tax-per-window tax-per-engine)
   (+ (* (get-wheels Vehicle) tax-per-wheel) (+ (* (get-windows Vehicle) tax-per-window) (* (get-engines Vehicle) tax-per-engine)))
)

(test (vehicle-tax myA 100 200 300) 2700)
(test (vehicle-tax myC 100 200 300) 500)

; Problem 6-c:
; Solved by myself: Y
; Time taken: about 10 mins
; [contract] is-vehicle-safe : Vehicle -> string["safe", "unsafe"]
; [purpose] To consumes a Vehicle instance and returns a string "safe" or "unsafe"
; [tests] (test (is-vehicle-safe myA) "safe")
;         (test (is-vehicle-safe myC) "unsafe")

(define (is-vehicle-safe Vehicle)
  (cond
    [(Bycycle? Vehicle)
     (cond
       [(< (get-wheels Vehicle) 4) "safe"]
       [else "unsafe"])]
    [(Car? Vehicle)
     (cond
       [(and (>= (get-wheels Vehicle) 4) (>= (get-windows Vehicle) 2)) "safe"]
       [else "unsafe"])]
   [(Airplane? Vehicle)
     (cond
       [(and (>= (get-engines Vehicle) 1) (and (>= (get-wheels Vehicle) 2) (>= (get-windows Vehicle) 10))) "safe"]
       [else "unsafe"])]
))

(test (is-vehicle-safe myA) "safe")
(test (is-vehicle-safe myC) "unsafe")


; Problem7:
; Solved by myself: N (I saw document and example code from
;                      for/list : https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._for%2Flist%29%29
;                      equal : https://docs.racket-lang.org/reference/Equality.html#%28def._%28%28quote._~23~25kernel%29._eq~3f%29%29
;                      example code : https://stackoverflow.com/questions/40119385/racket-error-in-string-replace-function )
; Time taken: about 180 mins
; [contract] update-name : target(string) replace(string) old(string) -> new(string)
; [purpose] To replace all occurences of old by new
; [tests] (test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
;         (test (update-name 'aaa 'bbb (cons 'aaa (cons 'ccc (cons 'ddd empty)))) '(bbb ccc ddd))
(define (update-name target replace old)
   (for/list ([item old])
     (if (equal? item target)
         replace
         item)
     )
)

(test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
(test (update-name 'aaa 'bbb (cons 'aaa (cons 'ccc (cons 'ddd empty)))) '(bbb ccc ddd))


; Problem 8:
; Solved by myself: N (I saw document
;                      list-ref : https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29
;                      floor : https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._floor%29%29
;                      drop-right : https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._drop-right%29%29
;                      take-right : https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take-right%29%29)
; Time taken: about 300 mins
; [contract] binary-search : list(int), number(int), list(int) -> list(int)
; [purpose] To return binary-earch traversal history
; [tests] (test (binary-search '(1 2 3) 2 '()) '(2))
;         (test (binary-search '(1 2 3 4 5) 3 '()) '(3))

(define (length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (length (rest lst)))] ))

(define (binary-search lst target res)
   (cond
    [(= (list-ref lst (floor (/ (length lst) 2))) target)
        append(list res) (list target) ]
    [(> (list-ref lst (floor (/ (length lst) 2))) target)
       (binary-search (drop-right lst (floor (/ (length lst) 2)))
                      target
                      (append(list res) (list (floor (/ (length lst) 2)) )) )] ;foward
    [else
       (binary-search (take-right lst (floor (/ (length lst) 2)))
                      target
                      (append(list res) (list (floor (/ (length lst) 2)) )) )] ;backward
    )
)

(test (binary-search '(1 2 3) 2 '()) '(2))
(test (binary-search '(1 2 3 4 5) 3 '()) '(3))
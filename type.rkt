#lang plai

#|
;*** type definition ***
(define-type Animal
  (bear (num_child number?)
        (colot string?))
       (giraffe (height number?)
                (name string?)
                (len_neck number?))
        (snake (lenth number?)
               (poision boolean?))
 )

(define myBear (bear 2 "white"))
(define myGiraffe (giraffe 200 "GGG" 100))
(define mySnake (snake 100 true))

myBear ;(bear 2 "white"))
(Animal? myBear) ;#t
(bear? myBear) ;#t
(snake? myBear) ;#f

; *** type deconstruction ***

(define (getnumber a)
  (type-case Animal a
    [bear (n c) n]
    [giraffe (h n l_n) (list h l_n)]
    [snake (l p?) true])
 )

(getnumber myBear) ;2
(getnumber myGiraffe) ;'(200 100)
(getnumber mySnake) ;#t


; ** list ***
(list 'a 'b 'c) ;'(a b c)
null ;'()

(list empty (list 1 2 3)) ;'(() (1 2 3))2


(cons 1 empty) ;'(1) =>(list 1)
(cons 'a(cons 2 empty)) ;'(a 2) => (list 'a 2)

(list 1 2 3) ;'(1 2 3) => (list 1 2 3)
(list 1 2 3 empty) ; '(1 2 3 ()) => (list 1 2 3 empty)

(append (list 1 2) empty) ; '(1 2) => (list 1 2)
(append (list 1 2) ; '(1 2 3 4) => (list 1 2 3 4)
        (list 3 4))
(append (list 1 2) ; '(1 2 a b #t) => (list 1 2 'a 'b true)
        (list 'a 'b)
        (list true))

(first (list 1 2 3))
(rest (list 1 2 3))
(first (rest (list 1 2))

;'(...) creates a list. it distributes over elements:
'(1 2 3)
'(a b)
'((a 2) (3 4))
'10 ; 10, just number

(cons 1 2) ;'(1 . 2) => '(1 . 2), which is a non-list pair

(empty? empty) ;t
(empty? (cons "head" empty)) ;f
(cons? empty) ;f
(cons? (cons "head" empty)) ;t

|#

; *** recursion ***
(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))] ))

(my-length '(a b c)) ;3
(my-length empty) ;0
              
               
                      
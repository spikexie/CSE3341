;this function is used to detemine wether this element x is in the list l
;para1 $x: the element need to be determined
;para2 $l: the list is used to be checked
(define (xmemb ele l)  
;which is the if...elif...else statement in python
    (cond
    ;if the list $l is null, then return false
		((null?  l)  #f)
    ;if the first element in $l is $x, then return true
		((eq?  ele  (car  l)) #t)  
    ;otherwise go check the rest of the element in $l
    (else (xmemb ele (cdr l) ) )
	)
)

; this function is used to check what are the common element in both list l1 and l2
; para1 $l1: the first list
; para $l2: the second list
(define (commonElement l1 l2)
;which is the if...elif...else statement in python
	(cond
  ;if the list $l1 is null, then return false
    ((null? l1) '()) 
     ;if the list $l1 is null, then return false
		((null? l2) '())
     ;check if the first element in l1 is in l2, then construct a new list
      ;whcih is used to contain all the elements which in l1 are also in l2
    ((xmemb (car l1) l2) (cons (car l1) (commonElement (cdr l1) l2))) 
     ;otherwise go check the rest elements in l1
    (else (commonElement (cdr l1) l2))) 
)

; this function is used to sort all element in l1 in non decreasing order, return the smallest elemnt in l1
; para1 $l1: the list needed to be sort
; para2 $ele: the element used to determine the sequence
(define (sortIncreasing l1 ele)
 ;which is the if...elif...else statement in python
	(cond 
  ;if the list $l1 is null, then return $ele
		((null? l1) ele) 
    ;to detemine the sequence, if the first element in l1 is smaller than $ele then
    ;repeat this method until we find the smallest
	  ((< (car l1) ele) (sortIncreasing (cdr l1) (car l1))) 
      ;otherwise repeat this method
	  (else (sortIncreasing (cdr l1) ele))
	)
)

; this function is used to remove an element in l1
; para1 $l1: the list we want to check
; para2 $ele: the element we want to remove from the list
(define (delete l1 ele)
 ;which is the if...elif...else statement in python
	(cond 
  ;if the list $l1 is null, then return an empty list
		((null? l1) '()) 
    ; if the first element is equal to $ele, then return the rest element of $l1
    ((eqv? (car l1) ele) (cdr l1)) 
    ; otherwise we repesat this function
    (else (cons (car l1) (delete (cdr l1) ele))) 
  	)
)

; this function is used to sort a list in non decreasing order
(define (sort l1)
;which is the if...elif...else statement in python
	(cond
  ;if the list $l1 is null, then return an empty list
		((null? l1) '())
    ; sort all elements in $l1
		(else (cons (sortIncreasing l1 (car l1)) (sort (delete l1 (sortIncreasing l1 (car l1)))))) 
	)
)

; this function we want to find the negation of two lists
; para1 $l1: the first list
; para2 $l2: the second list
(define (inAnotB l1 l2)
;which is the if...elif...else statement in python
	(cond
  ;if the list $l1 is null, then return an empty list
		((null? l1) '()) 
    ;if the list $l2 is null, then return $l1
	 	((null? l2) l1)
     ; if the first element of $l1 is in $l2, then we check the rest elements of $l1
		((xmemb (car l1) l2) (inAnotB (cdr l1) l2)) 
    ; then we construct a new list which we want to contain all the elements in $l1 but not in $l2
		(else (cons (car l1) (inAnotB (cdr l1) l2)))
	)
)
; this function combines all functions to finish our job that do the negation of two lists and then sort the new list in non decreasing order
(define (eliminateNsort l1 l2)
	(sort (inAnotB l1 (commonElement l1 l2))) 
)
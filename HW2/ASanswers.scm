;1


(define (is-list? x)
  (cond ((eqv? x '()) #t) ; checking for empty list
        ; if x is pair, keep checking rest recursivley
        ((pair? x) (is-list? (cdr x))) 
        ( #f))) ; if not a list, return false.




;2 - 2.18 page 103

(define (my-reverse x)
    (cond ((not (list? x)) x) ; if list not a list, return the element passed
          ((null? x) '()) ; if the list is null, return empty list. 
          ; else - recursion to reverse the list. 
          (else (append (my-reverse (cdr x)) (list (my-reverse (car x)))))))



;3 - 2.20 page 104
; filter procedure from SICP
(define (filter predicate sequence)
  (cond ((null? sequence) '()) 
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (same-parity head . tail)
  (cons head 
        (filter (if (even? head) ; will filter based
                    even?        ; if even parity or odd
                    odd?)
                tail)))


; 4 - 2.21 page 106

(define (square-list items)
    (if (null? items)
        '()
       ; recursivley squares the car and cons it to
        ; remaining cdr list.Rest of the list is processed
        ; and squared using tail recursion.
        (cons (expt (car items) 2) (square-list (cdr items)))))


; procedure to square an element. 
(define (square x)
  (* x x))

; apply the square procedure to
; the list items and you square
; the elements inside of the list. 
(define (square-list items)
   (map square items))
   


; 5 - 2.23 page 107

(define (my-for-each proc items) 
  (cond ((not (null? items)) 
         (proc (car items)) 
         (my-for-each proc (cdr items)))))


; 7 - 2.54 page 145

(define (my-equal? x y)
  (cond ((null? x) (null? y)) ; return nothing
        ((null? y) (null? x)) ; return nothing
        ; check recursively for equal lists. 
        ((eq? (car x) (car y)) (my-equal? (cdr x) (cdr y)))
        (else #f)))

; this test fails
(my-equal? '(2) '(2))


;8 Part 1

; if the filter is able to create a new "filtered list"
; that means that the sequence, a list in this case,
; satisfies the predicate.
(define (every? predicate sequence)
  (not (null? (filter predicate sequence))))

; part 2
;  If the list is empty then every? should return #f if the predicate is not
; null? or list?. The only way for an empty list to be true is if it's tested
; with the predicates null? or list?. If the empty list is tested with any other
; predicate, such as number?, it should return false, since the empty list is
; not a number. So to reiterate, an empty list should return true in the
; procedure every if and only if the predicate is null? or list?


;9 - exercise 2.59 page 153

; check if element if part of set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (unordered-union-set set1 set2) 
   (cond ((null? set1) set2) 
         ((null? set2) set1) 
         ((element-of-set? (car set1) set2) ; checking for duplicates
          (unordered-union-set (cdr set1) set2))
         ; the union of the unordered-set is done below
         (else (unordered-union-set (cdr set1) (cons (car set1) set2)))))   


;10 exercise 2.62 page 155


(define (ordered-union-set set another)
    (cond ((and (null? set) (null? another))
            '()) ; if both sets are empty, return null.
          ((null? set)
            another)
          ((null? another)
            set)
          (else
            (let ((x (car set)) (y (car another)))
              ; element comparisons are made below, and the list is created
              ; in the order of elements, from least to greatest. The cons
              ; helps in creating the ordered set. 
                (cond ((= x y)
                        (cons x (ordered-union-set (cdr set) (cdr another))))
                      ((< x y)
                        (cons x (ordered-union-set (cdr set) another)))
                      ((> x y)
                        (cons y (ordered-union-set set (cdr another)))))))))



;11 


; I use a filter to to create a new list
; with the indicated element removed in the
; new list.
(define (remove-val obj list1)
  (filter (lambda (x) (not (equal? x obj))) list1))



(define (my-equal? x y)
  (cond ((null? x) (null? y)) ; return nothing
        ((null? y) (null? x)) ; return nothing
        ; check recursively for equal lists.
        ((eqv? (car x) (car y)) (my-equal? (cdr x) (cdr y)))
        (else #f)))


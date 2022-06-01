(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))
(define source (with-input-from-file "units.dat" read-file))

; Procedure return the power
; of each element in the lists
; provided to the convert procedure. 
(define (gp l)
  (+ (cadr l) 0)
  )


; procedure for adding powers. 
(define (power-adder p)

  (if (equal? (length p) 2)
      (+ (cadr (car p)) (cadr (cadr p)))

      (if (equal? (length p) 3)
          (+ (cadr (car p)) (cadr (cadr p)) (cadr (caddr p)))
  )))

; this procedure helps me get the numerical
; value of the elementary unit. Useful for
; getting elementary units for something like hp
; or joules. 
(define (baseU-pow p)
 ; for hp send to another function. 
 ; need or for kg, m, and sec.
 (if (not (assoc (car p) source)) 
     1

     (let ((b (car p))); furlong
       (let ((a (cadr (assoc b source))))

         (if (equal? (length (cdr a)) 2)
             (power-adder (cdr a))
             (if (equal? (length (cdr a)) 3)
                 (power-adder (cdr a))
                 ; * will get units elemenatry units.(list length 1)
                 (cadr (car (cdr a))) 
                 ))))))

; check for compatible powers;
; to see if they have the same powers. 
; below are 9 different versions of
; p-match, due to different size of
; list input in convert;
; p-match stands for power match. 
(define (p-match1-1 from-quan to-quan)

  ; power check for 1-1 - ex: (convert '(4 (in 1)) '((ft 1)))
  ; ignores the numerical value in front of unit in quantity list.
  (let ((a (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))))
    (let ((b (* (gp (car to-quan))(baseU-pow (car to-quan)))))

      (cond ((equal? a b) #t)
            (else #f))
      )))

; 1-2, for case where quanity list length is 1 and unit list length is 2
(define (p-match1-2 from-quan to-quan)

  ; power check for 1-2 ex: 
  (let ((a (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))))
    (let ((b (+ (* (gp (car to-quan))(baseU-pow (car to-quan)))
                (* (gp (cadr to-quan)) (baseU-pow (cadr to-quan))))))
      (cond ((equal? a b) #t)
            (else #f))
      )))

; 1-3 for case where quantity list length is 1 and unit list length is 3
(define (p-match1-3 from-quan to-quan)
  
  (let ((a (+ (*(gp (car to-quan))(baseU-pow (car to-quan)))              
              (*(gp (cadr to-quan)) (baseU-pow (cadr to-quan)))
              (*(gp (caddr to-quan)) (baseU-pow (caddr to-quan))))))
    (let ((b (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))))

      (cond ((equal? a b) #t)
            (else #f))
      )))

; 2-1 for case where quantity list length is 2 and unit list length is 1 
(define (p-match2-1 from-quan to-quan)

   ; power check for 2-1
  (let ((b (* (gp (car to-quan))(baseU-pow (car to-quan)))))
  (let ((a (+ (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))
              (* (gp (caddr from-quan)) (baseU-pow (caddr from-quan))))))
 
  (cond ((equal? a b) #t)
        (else #f))
  )))

; 2-2 power match for quantity list length 2 and unit list length 2
(define (p-match2-2 from-quan to-quan)
  ;(display "hello")
  ; power check for 2-2
  (let ((a (+ (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))
              (* (gp (caddr from-quan)) (baseU-pow (caddr from-quan))))))
    
    (let ((b (+ (* (gp (car to-quan))(baseU-pow (car to-quan)))
                (* (gp (cadr to-quan)) (baseU-pow (cadr to-quan))))))

      (cond ((equal? a b) #t)
            (else #f))
  )))

; 2-3 for case where quantity list length is 2 and unit list length is 3 
(define (p-match3-3 from-quan to-quan)

  (let ((a (+ (* (gp (car to-quan))(baseU-pow (car to-quan)))
              (* (gp (cadr to-quan)) (baseU-pow (caddr to-quan)))
              (* (gp (caddr to-quan)) (baseU-pow (caddr to-quan))))))
    (let ((b (+ (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))
                (* (gp (caddr from-quan)) (baseU-pow (caddr from-quan)))))) 
      
      (cond ((equal? a b) #t)
            (else #f))
      )))

; 3-1 for case where quantity list length is 3 and unit list length is 1 
(define (p-match3-1 from-quan to-quan)
  
  ; power check for 3-1
  (let ((a (+ (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))
              (* (gp (caddr from-quan)) (baseU-pow (caddr from-quan)))
              (* (gp (cadddr from-quan)) (baseU-pow (cadddr from-quan))))))
    (let ((b (* (gp (car to-quan))(baseU-pow (car to-quan)))))

      (cond ((equal? a b) #t)
            (else #f))
      )))

; 3-2 for case where quantity list length is 3 and unit list length is 2 
(define (p-match3-2 from-quan to-quan)
  
  ; power check for 3-2
  (let ((a (+ (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))
              (* (gp (caddr from-quan)) (baseU-pow (caddr from-quan)))
              (* (gp (cadddr from-quan)) (baseU-pow (cadddr from-quan))))))
    
    (let ((b (+ (* (gp (car to-quan))(baseU-pow (car to-quan)))
                (* (gp (cadr to-quan)) (baseU-pow (cadr to-quan))))))

      (cond ((equal? a b) #t)
            (else #f)))))

; 3-3 for case where quantity list length is 3 and unit list length is 3 
(define (p-match4-3 from-quan to-quan)
  
  ; power check for 3-3
  (let ((a (+ (* (gp (cadr from-quan))(baseU-pow (cadr from-quan)))
              (* (gp (caddr from-quan)) (baseU-pow (caddr from-quan)))
              (* (gp (cadddr from-quan)) (baseU-pow (cadddr from-quan))))))
    
    (let ((b (+ (* (gp (car to-quan))(baseU-pow (car to-quan)))
                (* (gp (cadr to-quan)) (baseU-pow (cadr to-quan)))
                (* (gp (caddr to-quan)) (baseU-pow (caddr to-quan))))))

      (cond ((equal? a b) #t)
            (else #f))
      )))

; This function will get the number quantity
; of each element in the lists
; for example it will get
; furlong = 206.168, cm =  0.01cm, etc.
(define (num-val l)
 
  (if (not (assoc (car l) source))
      1
      
  (let ((b (car l)))
    (let ((a1 (car (cadr (assoc b source)))))
      (+ a1 0))))) ; this helps return the number. 

; this procedure will help in retriving the elementary units
; of a derived unit. For example for hp it will return
; (kg m sec) only. The powers are removed. 
(define (rm-pow-lst l)

  (if (equal? (length l) 2)
      (cons (car (car l)) (cons (car (cadr l)) '()))

      (if (equal? (length l) 3)
      (cons (car (car l)) (cons (car (cadr l)) (cons (car (caddr l)) '()))) 
  
  )))

; this procedure helps me in getting the
; elementary units of each element in
; quantity and unit-list in the convert
; procedure. derived units are sent to
; the procedure above (rm-pow-lst l).
(define (elem-units l)
  (if (not (assoc (car l) source)) 
     (car l)

     (let ((b (car l))); furlong
       (let ((a (cadr (assoc b source))))

         (if (equal? (length(cdr a)) 2)
             (rm-pow-lst (cdr a)) ; for 3 cdr with length of 3.
             (if (equal? (length(cdr a)) 3)
                 (rm-pow-lst (cdr a))
                 (car (car (cdr a))) ; * will get base units of list length 1
                 ))))))

; the three procedures below help me in checking that
; the quantity list and unit list share the same base(elementary) units.
(define (list-match? list1 list2)
  (or (null? list1)
      (and (member (car list1) list2)
           (list-match? (cdr list1) list2))))

(define (match l1 l2)
   (and (list-match? l1 l2)
       (list-match? l2 l1)))

; helps removes nested list to make
; comparisons easier between list. 
(define (paren-m lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (paren-m (car lst))
                 (paren-m (cdr lst))))
        (else (cons (car lst) (paren-m (cdr lst))))))

; the procedures below checks to see if the
; elements have compatible
; elementary units (m, kg, sec)
; there are 9 if statements that take into account
; the size of the quanity list and unit list. 
(define (check-elem from to)
  
  ;check for 1-1 - s
  (if (and (equal? (length from) 2) (equal? (length to) 1))
      (let ((a (cons (elem-units (cadr from)) '())))
        (let ((b (cons (elem-units (car to)) '())))
          (match (paren-m a) (paren-m b))))
          
  ;1-2 - s
  (if (and (equal? (length from) 2) (equal? (length to) 2))
      (let ((a (cons (elem-units (cadr from)) '())))
        (let ((b (cons (elem-units (car to))
                       (cons (elem-units (cadr to)) '()))))
          (match (paren-m a) (paren-m b))))
                   
  ; 1-3 - s
  (if (and (equal? (length from) 2) (equal? (length to) 3))
      (let ((a (cons (elem-units (cadr from)) '())))
        (let ((b (cons (elem-units (car to))(cons (elem-units (cadr to))
                  (cons(elem-units (caddr to)) '())))))
          (match (paren-m a) (paren-m b)))) 

  ; 2-1 - s
  (if (and (equal? (length from) 3) (equal? (length to) 1))
      (let ((a (cons (elem-units (cadr from))
                     (cons (elem-units (caddr from))'()))))
        (let ((b (cons (elem-units (car to))
                       (cons (elem-units (car to)) '()))))
          (match (paren-m a) (paren-m b))))
     
  ; 2-2 - s
  (if (and (equal? (length from) 3) (equal? (length to) 2))  
      (let ((a (cons (elem-units (cadr from))
                     (cons (elem-units (caddr from))'()))))
        (let ((b (cons (elem-units (car to))
                       (cons (elem-units (cadr to)) '()))))
          (match (paren-m a) (paren-m b))))
       
  ;2-3 - s
  (if (and (equal? (length from) 3) (equal? (length to) 3))
      (let ((a (cons (elem-units (cadr from))
                     (cons (elem-units (caddr from))'()))))
        (let ((b (cons (elem-units (car to))
                       (cons (elem-units (cadr to))
                             (cons(elem-units (caddr to)) '())))))
          (match (paren-m a) (paren-m b))))     
  
  ; 3-1  
  (if (and (equal? (length from) 4) (equal? (length to) 1))
      (let ((a (cons (elem-units (cadr from))
                     (cons (elem-units (caddr from))
                           (cons(elem-units (cadddr from)) '())))))        
        (let ((b (cons (elem-units (car to))'())))
          (match (paren-m a) (paren-m b))))     
    
  ; 3 - 2 
  (if (and (equal? (length from) 4) (equal? (length to) 2))
      (let ((a (cons (elem-units (cadr from))
                     (cons (elem-units (caddr from))
                           (cons(elem-units (cadddr from)) '()))))) 
        (let ((b (cons (elem-units (car to))
                       (cons (elem-units (cadr to)) '()))))
    (match (paren-m a) (paren-m b))))

   ; 3-3
   (if (and (equal? (length from) 4) (equal? (length to) 3))
      (let ((a (cons (elem-units (cadr from))
                     (cons (elem-units (caddr from))
                           (cons(elem-units (cadddr from)) '())))))
        (let ((b (cons (elem-units (car to))
                       (cons (elem-units (cadr to))
                             (cons(elem-units (caddr to)) '()))))) 
    (match (paren-m a) (paren-m b))))   
  ))))))))))



; will return the numerical value
; of the unit raised to its power;
; example:(cm 2) will return 0.0001
; This makes calculations easier for me. 
(define (f-num element)
  ;(display (element))
  (cond ((or (memq 'kg element) (memq 'm element) (memq 'sec element))  
      (expt 1 (gp element)))
      (else (expt (num-val element) (gp element))))
  )
; The procedure below will return the numerical value of the
; final answer. It is case dependent like the above check elem.
(define (calculation from to)
  
   ; calculations for list lenths 1 and 1
  (if (and (equal? (length from) 2) (equal? (length to) 1))
      (* (car from) (/ (f-num (cadr from)) (f-num(car to))))

  ; calculations for list lenths 1 and 2
  (if (and (equal? (length from) 2) (equal? (length to) 2))
      (* (car from) (/ (f-num (cadr from))
                       (* (f-num (car to)) (f-num (cadr to)))))

  ; calculations for list lenths 1 and 3
  (if (and (equal? (length from) 2) (equal? (length to) 3))
       (* (car from) (/ (f-num (cadr from))(*(f-num (car to))(f-num (cadr to))
                                             (f-num(caddr to)))))  
   ; calculations for list lenths 2 and 1
  (if (and (equal? (length from) 3) (equal? (length to) 1))
      (* (car from)(/ (* (f-num (cadr from)) (f-num (caddr from)))
                      (f-num (car to))))

   ; calculations for list lenths 2 and 2
  (if (and (equal? (length from) 3) (equal? (length to) 2))
      (* (car from)(/ (* (f-num (cadr from)) (f-num (caddr from)))                      
                      (* (f-num (car to)) (f-num (cadr to)))))

  ; calculations for list lenths 2 and 3
  (if (and (equal? (length from) 3) (equal? (length to) 3))
      (* (car from)(/ (* (f-num (cadr from)) (f-num (caddr from)))
                      (* (f-num (car to)) (f-num (cadr to))(f-num (caddr to)))))
      
   ; calculations for list lenths 3 and 1
   (if (and (equal? (length from) 4) (equal? (length to) 1))
      (* (car from)(/ (* (f-num (cadr from)) (f-num (caddr from))
                         (f-num (cadddr from)))(* (f-num (car to)) 1)))
      
  ; for 3-2
  (if (and (equal? (length from) 4) (equal? (length to) 2))      
      (* (car from)(/ (* (f-num (cadr from)) (f-num (caddr from))                         
                         (f-num (cadddr from)))
                      (* (f-num (car to)) (f-num (cadr to))))) ; too long
      
  (if (and (equal? (length from) 4) (equal? (length to) 3))      
     (* (car from)(/ (* (f-num (cadr from))
                        (f-num (caddr from))(f-num (cadddr from)))
                     (* (f-num (car to)) (f-num (cadr to))(f-num(caddr to)))))

))))))))))

; will show final numerical quantity
; and units converted too.
; will display the final answer. 
(define (final-list number to)
     (cons number to)
  )
                                        
; this is the function that will
; turn a quantity into another quantity.
; it will take two list as inputs.
(define (convert from-quan to-quan)

  ;(display (check-elem from-quan to-quan))
  ;(display (p-match2-2 from-quan to-quan)) ; test for 4-3
  
  ; 1-1
  (if (and (equal? (length from-quan) 2) (equal? (length to-quan) 1))
      (cond ((equal?(check-elem from-quan to-quan)
                    (p-match1-1 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))             
            (else (display "Not Compatible")))
  ;1-2
  (if (and (equal? (length from-quan) 2) (equal? (length to-quan) 2))
      (cond ((equal?(check-elem from-quan to-quan)
                    (p-match1-2 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))             
            (else (display "Not Compatible")))
  ;1-3
  (if (and (equal? (length from-quan) 2) (equal? (length to-quan) 3))
      (cond ((equal?(check-elem from-quan to-quan)
                    (p-match1-3 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))              
            (else (display "Not Compatible")))
 
  ;2-1
  (if (and (equal? (length from-quan) 3) (equal? (length to-quan) 1))
      (cond ((equal?(check-elem from-quan to-quan)
                    (p-match2-1 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))             
            (else (display "Not Compatible")))
      
  ;2-2
  (if (and (equal? (length from-quan) 3) (equal? (length to-quan) 2))
      (cond ((equal?(check-elem from-quan to-quan)
                    (p-match2-2 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))             
            (else (display "Not Compatible")))

 ; for 2-3
   (if (and (equal? (length from-quan) 3) (equal? (length to-quan) 3))
       (cond ((equal? (check-elem from-quan to-quan)
                      (p-match3-3 from-quan to-quan))
              (final-list (calculation from-quan to-quan) to-quan))
             (else (display "Not Compatible")))
      
  ; for 3-1
   (if (and (equal? (length from-quan) 4) (equal? (length to-quan) 1))
       (cond ((equal? (check-elem from-quan to-quan)
                      (p-match3-1 from-quan to-quan))
              (final-list (calculation from-quan to-quan) to-quan))
             (else (display "Not Compatible")))
  
  ; for 3-2
  (if (and (equal? (length from-quan) 4) (equal? (length to-quan) 2))           
      (cond ((equal? (check-elem from-quan to-quan)
                     (p-match3-2 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))
            (else (display "Not Compatible")))

  ; for 3-3
  (if (and (equal? (length from-quan) 4) (equal? (length to-quan) 3))
      (cond ((equal? (check-elem from-quan to-quan)
                     (p-match4-3 from-quan to-quan))
             (final-list (calculation from-quan to-quan) to-quan))
            (else (display "Not Compatible")))

  (if (or (> (length from-quan) 4) (> (length to-quan) 3))
      (display "not compatible")     
  )))))))))))

; get this to work and it will help me alot
; finishing, could be used as model to finish HW.
;******************* TEST AREA
; 1-1 - works - c
;(convert '(2 (ft 1)) '((m 1)))
; 1-2 - works
;(convert '(2 (cm 2)) '((in 1)(mm 1))) ; works
;2-1 - c
;(convert '(2 (in 1)(mm 1)) '((m 2)))
; 2-2 - works - c
;(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
; 2-3 - works - c
;(convert '(3 (cm 2)(kg 1)) '((in 1)(g 1)(in 1)))
; 3-2 - works - c
;(convert '(3 (in 1)(g 1)(in 1)) '((cm 2)(kg 1)))
; 3-1 - works - c
;(convert '(32999.99 (ft 1)(lbf 1)(min -1)) '((hp 1)))
; 1-3 works - c  
;(convert '(32999.99 (hp 1)) '((ft 1)(lbf 1)(min -1)))
; final 3-3 - c
;(convert '(23 (g -1)(m 1)(min 3)) '((joule 2)(N -3)(hr 1)))
;* do derived units(hp, joule) last, don't go nuts over this. 
; hardest part of that is simply base unit, everything else
; is rudimentary. 

; shouldnt be working but works somehow.
;(convert '(2 (cm 2)) '((in 1) (kg 1))) ; solution, additional base check cond

;(convert '(2 (ft 1)) '((m 2)))

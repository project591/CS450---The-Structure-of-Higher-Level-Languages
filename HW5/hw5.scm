(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;(define-syntax cons-stream
;  (syntax-rules ()
;    ((cons-stream x y)
;     (cons x (delay y)))))

; This will be used to display the stream upto a certain point n 
(define (display-n stream n)
  (cond ((or (< n 0) (equal? the-empty-stream stream)) (newline))
        ((display (stream-car stream))
         (newline)
         (display-n (stream-cdr stream) (- n 1)))))

(define (list-stream lst) ; will turn list to stream 
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst)             
                   (list-stream (cdr lst))))) 

(define (pow x) ; will take the power of a
  (expt 10 (- (length x) 1)))

; will turn a number, like 783, into a list like '(7 8 3)
(define (integer->list number) 
  (map (lambda (number) (- (char->integer number) 48))
       (string->list (number->string number))))

; procedure to create a new-alist
(define (next-alist a-list a m strm) 
  (integer->list (+ (* 10 a) (* m (stream-car (list-stream strm))))))

; if the old list is greater than or equal in length
; to the newer list, then a zero is prepended to new list.
(define (prepend new-list old-list)
  (cond ((> (length old-list) (length new-list))
         (append '(0) new-list))
        ((equal? (length old-list) (length new-list))
         (append '(0) new-list))
        ; if no prepend is needed then append
        ; the empty list to the newer list. 
        ((append '() new-list)))) 

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (action a a-list m strm)
  (let ((a-power (pow a-list))) 
    (cond ((equal? the-empty-stream strm) ; if the stream is empty
           (cons-stream (car a-list) (list-stream (cdr a-list))))
          ((and (> (length a-list) 0) (< (+ m (modulo a a-power)) a-power))
           ; PRODUCE!
           ; the car of a-list will be in the output stream
           ; call action recursively with
           ; a = (modulo a-power)
           ; a-list - (cdr a-list)
           ; same stream is passed.
           (cons-stream (car a-list) (action (modulo a a-power)
                                             (cdr a-list) m strm))
           )
        
          ; CONSUME!
          (else
           ; a - return it changed with equation done
           ; (a-list) - prepend 0 when needed. need to put in function 
           ; (display (+ (* 10 a) (* m (cadr a-list))))

           ; a is changed to below - will be passed to action
           (let ((a_2 (+ (* 10 a) (* m (stream-car (list-stream strm))))))  
             ;(newline)
             ;(display-n (stream-cdr strm) 2)
             (let ((list2 (next-alist a-list a m strm)))
       
               ;(display "display consume new list: ")      
       
               ;(newline)

               ; here I check if the a-list will need
               ; to be prepended with a zero
               (let ((newer-list (prepend list2 a-list))) ; prepend if needed
                 ; (display newer-list)
                 ; (newline)
                 ; (newline)
       
                 (action a_2 newer-list m (stream-cdr strm)))))))))

(define (mult-stream m strm)  
  (let ((str (list-stream strm))) ; turning the list into a stream   
    (action 0 '() m str)))

; tested using code below.
;(display-n (mult-stream 87 '(9 8 7 4 3 6 9 1 7)) 16)

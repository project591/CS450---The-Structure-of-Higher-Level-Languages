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

; defining cons-stream here
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

 
; This will be used to display the stream upto a certain point n 
(define (display-n stream n)
  (cond ((or (< n 0) (equal? the-empty-stream stream)) (newline))
        ((display (stream-car stream))
         (newline)
         (display-n (stream-cdr stream) (- n 1)))))

;#2 3.50, used here
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map 
               (cons proc (map stream-cdr argstreams))))))


; this could be thought of as the constructor for
; the many Matrices that will be defined in this assignment.
; This will initialize a Matrix.
(define (matrix a b c d)
  (list a b c d))

; I also use this, and four other procedures
; below as selectors for what rows
; and columns will be added in a Matrix
(define (add-selector-row1-1 matrix1 matrix2)
  (+ (car matrix1) (car matrix2)))

; another helper function to add matrices
; with corresponding matrix element. 
(define (add-selector-row1-2 matrix1 matrix2)
  (+ (cadr matrix1) (cadr matrix2)))

; another helper function to add matrices
; with corresponding matrix element. 
(define (add-selector-row2-1 matrix1 matrix2)
  (+ (caddr matrix1) (caddr matrix2)))
; another helper function to add matrices
; with corresponding matrix element. 
(define (add-selector-row2-2 matrix1 matrix2)
  (+ (cadddr matrix1) (cadddr matrix2)))

; procedure for adding matrices. 
(define (add-matrix m1 m2)

  (let ((a (add-selector-row1-1 m1 m2)))
  (let ((b (add-selector-row1-2 m1 m2)))
  (let ((c (add-selector-row2-1 m1 m2)))
  (let ((d (add-selector-row2-2 m1 m2)))
  (matrix a b c d))))))

; The selector functions below will be used
; for multiplying corresponding rows and columns with
; each other. There are four similar functions right below
; here that will be used for multiplying 2 matrices. 
(define (m-selector-row1-1 matrix1 matrix2)
  (+ (*(car matrix1) (car matrix2)) (*(cadr matrix1)(caddr matrix2))))

; a selector for multiypling matrices
(define (m-selector-row1-2 matrix1 matrix2)
  (+ (*(car matrix1) (cadr matrix2)) (*(cadr matrix1)(cadddr matrix2))))

; a selector for multiypling matrices
(define (m-selector-row2-1 matrix1 matrix2)
  (+ (*(caddr matrix1) (car matrix2)) (*(cadddr matrix1)(caddr matrix2))))

; a selector for multiypling matrices
(define (m-selector-row2-2 matrix1 matrix2)
   (+ (*(caddr matrix1) (cadr matrix2)) (*(cadddr matrix1)(cadddr matrix2))))

; procedure to multiply 2 matrices
(define (compose matrix1 matrix2)

  (let ((a (m-selector-row1-1 matrix1 matrix2)))
  (let ((b (m-selector-row1-2 matrix1 matrix2)))
  (let ((c (m-selector-row2-1 matrix1 matrix2)))
  (let ((d (m-selector-row2-2 matrix1 matrix2)))

    (matrix a b c d)
   )))))


; infinite streams of matrix (1 4 0 2) 
(define adder (cons-stream (matrix 1 4 0 2) adder))

; generating the infitine stream here.
; Original Input Stream
(define original-stream (cons-stream (matrix 1 6 0 3)
                              (stream-map add-matrix adder original-stream)))

(define (pi)
  (define (action a stream)

    ; the next four helper procedure below are for calculating the floor value.
    (define (floor1)
      (+ (* (car a) 3) (cadr a))
      )

    (define (floor2)
      (+ (* (caddr a) 3) (cadddr a))
      )

    
    (define (floor3)
      (+ (* (car a) 4) (cadr a))
      )

    (define (floor4)
      (+ (* (caddr a) 4) (cadddr a))
      )

    (define (produce)
      (cons-stream
       (quotient (floor3) (floor4))
       (action (compose (matrix 10
                                (* (quotient(floor3) (floor4)) -10) 0 1) a)
               stream)))
    
    ; cond check for floor calculation
    (cond ((equal? (quotient (floor1) (floor2)) (quotient (floor3) (floor4)))
           ;PRODUCE
           (produce)
           )                                    
          ; CONSUME
          (else (action (compose a (stream-car stream)) (stream-cdr stream)))))
  (action (matrix 1 6 0 3) (stream-cdr original-stream))
  )

; I used display-n to test the
; proc
;(display-n (pi) 10)

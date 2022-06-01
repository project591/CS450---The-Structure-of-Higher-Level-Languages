; https://www.cs.umb.edu/~offner/cs450/hw6/hw6.html
;;; file: s450.scm
;;;
;;; Metacircular evaluator from chapter 4 of STRUCTURE AND
;;; INTERPRETATION OF COMPUTER PROGRAMS (2nd edition)
;;;
;;; Modified by kwn, 3/4/97
;;; Modified and commented by Carl Offner, 10/21/98
;;;
;;; This code is the code for the metacircular evaluator as it appears
;;; in the textbook in sections 4.1.1-4.1.4, with the following
;;; changes:
;;;
;;; 1.  It uses #f and #t, not false and true, to be Scheme-conformant.
;;;
;;; 2.  Some function names were changed to avoid conflict with the
;;; underlying Scheme:
;;;
;;;       eval => xeval
;;;       apply => xapply
;;;       extend-environment => xtend-environment
;;;
;;; 3.  The driver-loop is called s450.
;;;
;;; 4.  The booleans (#t and #f) are classified as self-evaluating.
;;;
;;; 5.  And these modifications make it look more like UMB Scheme:
;;;
;;;        The define special form evaluates to (i.e., "returns") the
;;;          variable being defined.
;;;        No prefix is printed before an output value.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 xeval and xapply -- the kernel of the metacircular evaluator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

; might need this for UMB scheme. 
;(define (assoc key records)
;  (cond ((null? records) false)
;        ((equal? key (caar records)) (car records))
;        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  (display key)
  (newline))

(define (make-table)
  (list '*table*))

; this is the 1-D table that will hold
; the special forms. 
(define special-table (list '*table*))

(define (type-of exp)
 
  (if (pair? exp)
      (car exp)
      '())
  )

; will check if the expression is
; <eof> 
(define (eof-test exp)

  (eof-object? exp)
  
  )

(define (xeval exp env)
  (let ((action (lookup (type-of exp) special-table)))
    (if action
        (action exp env)
        (cond ((self-evaluating? exp) exp)
              ((variable? exp) (lookup-variable-value exp env))
              ((eof-object? exp) (eof-test exp))
              ((application? exp)
            
               (xapply (xeval (operator exp) env)
                       ; Will evaluate a user-defined-procedure
                       ; differently to check if it has
                       ; any tagged expressions in procedures
                       (cond ((user-defined-procedure? (xeval
                                                        (operator exp) env))
                              (list-of-values-other
                               (cadr (xeval (operator  exp) env))
                               (operands exp) env))
                                                            
                             (
                              
                            ;if the procedures is not a user-defined-procedures?
                            ; then evaluate in normal way.
                              (list-of-values (operands exp) env)

                              ))))
              (else
               (s450error "Unknown expression type -- XEVAL " exp))))))



; this is the dynamic environment
; will be used for when evaluating
; procedures that contain the dynamic tag
(define the-dynamic-environment '())


(define (xapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((user-defined-procedure? procedure)
         ; using let to save the value of the procedure.
         (let ((saved-value 
                (eval-sequence
                 (procedure-body procedure)
                 (xtend-environment                 
                  (procedure-parameters procedure)
                  arguments
                  (procedure-environment procedure)))))

           ; setting the dynamic environment to cdr of
           ; the dynamic environment. 
           (set! the-dynamic-environment (cdr the-dynamic-environment))

           ; returning the value so that it does not get lost.
           ; this ensures that xapply returns the value of the procedure. 
           saved-value))
        (else
         (s450error
          "Unknown procedure type -- XAPPLY " procedure))))

;;; Handling procedure arguments

(define (list-of-values exp env)
  (if (no-operands? exp)
      '()
      (cons (xeval (first-operand exp) env)
            (list-of-values (rest-operands exp) env))))

; other list of values

; evaluating dynamic arguments.
; this is where dynamic values are evaluated.
; notice that the environment used for the
; evaluation here is the the-dynamic-environment. 
(define (evaluate-dynamic param exp env)
      (cons (xeval (first-operand exp) the-dynamic-environment)
       (list-of-values-other (cdr param) (rest-operands exp) env)))

; evaluating delayed arguments
; this is where delayed arguments are evaluated
; notice that the evaluation involves creating
; a thunk of the parameter that contains a tag
; delayed.
(define (evaluate-delay param exp env)
  (cons (list 'thunk (first-operand env)) (list-of-values-other
            (cdr param) (rest-operands exp) env)))     

; this procedure is used to check what tag the expresion
; contains. The the expression will be evaluated depending
; if it contains either the tags delayed or dynamic.
(define (list-of-values-other param exp env)

  (if (no-operands? exp) 
      '()

      (if (dynamic? (car param)) ; seeing if expression contains dynamic tag      
          (evaluate-dynamic param exp env)
      
          (if (delayed? (car param)) ; seeing if expression contains delayed tag   
              (evaluate-delay param exp env)

              ; if expression contains no tags, then evaluate it in a normal way
              (cons
               (xeval (first-operand exp) env)  

               (list-of-values-other
                (cdr param) (rest-operands exp) env))))))


;;; These functions, called from xeval, do the work of evaluating
;;; special forms:
(define (eval-if exp env)
  (if (true? (xeval (if-predicate exp) env))
      (xeval (if-consequent exp) env)
      (xeval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (xeval (first-exp exps) env))
        (else (xeval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (let ((name (assignment-variable exp)))
    (set-variable-value! name
                         (xeval (assignment-value exp) env)
                         env)
    name))    ;; A & S return 'ok

(define (eval-definition exp env)
  (let ((name (definition-variable exp)))  
    (define-variable! name
      (xeval (definition-value exp) env)
      env)
    name))     ;; A & S return 'ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Representing expressions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numbers, strings, and booleans are all represented as themselves.
;;; (Not characters though; they don't seem to work out as well
;;; because of an interaction with read and display.)

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      ))

;;; variables -- represented as symbols

(define (variable? exp) (symbol? exp))

;;; quote -- represented as (quote <text-of-quotation>)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; this is almost the same as type-of-expression!
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;;; assignment -- represented as (set! <var> <value>)

(define (assignment? exp) 
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


;;; definitions -- represented as
;;;    (define <var> <value>)
;;;  or
;;;    (define (<var> <parameter_1> <parameter_2> ... <parameter_n>) <body>)
;;;
;;; The second form is immediately turned into the equivalent lambda
;;; expression.

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;; lambda expressions -- represented as (lambda ...)
;;;
;;; That is, any list starting with lambda.  The list must have at
;;; least one other element, or an error will be generated.

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; conditionals -- (if <predicate> <consequent> <alternative>?)

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;;; sequences -- (begin <list of expressions>)

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;;; procedure applications -- any compound expression that is not one
;;; of the above expression types.

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;;; Derived expressions -- the only one we include initially is cond,
;;; which is a special form that is syntactically transformed into a
;;; nest of if expressions.

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cdr exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f                          ; no else clause -- return #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (s450error "ELSE clause isn't last -- COND->IF "
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


; checking if expression has the
; "delayed" tag in it. 
(define (delayed? exp)
  (tagged-list? exp 'delayed))

; checking if expression has the
; "dynamic" tag in it. 
(define (dynamic? exp)
  (tagged-list? exp 'dynamic))

; from the book
(define (thunk? obj)
  (tagged-list? obj 'thunk))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Evaluator data structures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Truth values

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))


;;; Procedures

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (user-defined-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Representing environments
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An environment is a list of frames.

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;;; Each frame is represented as a pair of lists:
;;;   1.  a list of the variables bound in that frame, and
;;;   2.  a list of the associated values.

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))



;;; Extending an environment
(define (xtend-environment vars vals base-env)
  ; this inline procedure below helps in removing the tags
  ; in the body of the procedures
  (define (eliminate param)

    ; helper procedure to remove tags
    (define (tag-eraser parameter)
      (cadr parameter))

    ; cond to check if the paramter in the procedure
    ; contains any tags, so that they can be removed
    (cond ((null? param) '())
          ((delayed? (car param))
           (cons (tag-eraser (car param)) (eliminate (cdr param))))
          ((dynamic? (car param))
           (cons (tag-eraser (car param)) (eliminate (cdr param))))
          ((cons (car param) (eliminate (cdr param))))))
  
  (if (= (length vars) (length vals))
      (let*  ((normal-val (make-frame (eliminate vars) vals))
              (dynamic-val normal-val))

        ; pushing value into the dynamic environment
        ; here
        (set! the-dynamic-environment
              (cons dynamic-val the-dynamic-environment))

        ; the return value of xtend-environment will be the same
        (cons normal-val base-env)) 
      (if (< (length vars) (length vals))
          (s450error "Too many arguments supplied " vars vals)
          (s450error "Too few arguments supplied " vars vals))))


;(if (lookup var special-table)
;          var
;;; Looking up a variable in an environment
; for part 4, just lookup the table you created earlier
; it is a simple table search up.
(define (lookup-variable-value var env)  
  (define (env-loop env)
    (define (scan vars vals)
      ; part 4 done.
      (if (lookup var special-table)
          var          
          
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals))))))
    (if (eq? env the-empty-environment)
        (s450error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))



;;; Setting a variable to a new value in a specified environment.
;;; Note that it is an error if the variable is not already present
;;; (i.e., previously defined) in that environment.

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)   
       (if (lookup var special-table)      
          (reset)
      
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals))))))
    (if (eq? env the-empty-environment)
        (s450error "Unbound variable -- SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; Defining a (possibly new) variable.  First see if the variable
;;; already exists.  If it does, just change its value to the new
;;; value.  If it does not, define the new variable in the current
;;; frame.

; Helper procedure for when someone tries to define
; a special form. An error message is thrown when someone
; tries to define a special form;
; I try mimicking what happens in UMB Scheme when you try
; defining a special form. 
(define (reset)

  (display "ERROR: YOU CANNOT DEFINE A SPECIAL FORM: ")
  
  ; I call (s450) again to recover from error. 
  )

(define (define-variable! var val env)

  ; if someone is trying to define
  ; a special form, the reset procedure
  ; will be invoked. 
  (if (lookup var special-table)      
          (reset)
          
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame)))))

; I use a modified version of the
; lookup-variable-value procedure
; to look if a variable is defined.
; Instead of returning the value of a variable
; I return #t if a variable with value is found.
(define (defined? var env) 
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? (cadr var) (car vars))
             #t) ; if the variable is defined it will return #t
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        #f  ; if the variable is not defined in any environment, return #f
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; filter procedure from SICP textbook - section 2.2.2
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; procedure for locally-defined?
; I use a filter in the first frame
; if the filter extracts a variable
; equal to the expression passed,
; then the procedure will return #t
; if the filter returns an '(), then
; the procedure returns #f.
(define (locally-defined? exp env)

  ; working here with the first frame
  ; of the enviroment. 
  (let ((frame (car env)))
    
    ; flt stands for filtered list
    (let ((flt (filter (lambda (z) (equal? z (cadr exp))) (car frame))))
      
      (if (equal? flt '())
          #f ; if the filter returns '()
          #t ; if the filter finds a matching element return #t
))))


; this procedure uses filter to check if
; variable is in the first frame of the
; environment. If the variable is found in
; the first frame of the environment, the it
; is removed
(define (locally-make-unbound! var env)
  (define (locally-defined-internal? exp env)

    ; working here with the first frame
    ; of the enviroment. 
    (let ((frame (car env)))
    
      ; flt stands for filtered list
      (let ((flt (filter (lambda (z) (equal? z exp)) (car frame))))
      
        (if (equal? flt '())
            #f ; if the filter returns '()
            #t ; if the filter finds a matching element return #t
            ))))
  ; if it is found in locally-make-unbound, then set empty it.

  ;(display (car (car env)))
  
  (if (locally-defined-internal? var env)
     
      (begin
        (set-car! (car env)
                  (filter (lambda (x) (not (equal? var x))) (caar env)))
        (set-cdr! (car env)
                  (filter (lambda (x) (not (equal? var x))) (caar env)))
        )    
      ))  
  
(define (local-eval-unbind exp env)
  (locally-make-unbound! (car (operands exp)) env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The initial environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is initialization code that is executed once, when the the
;;; interpreter is invoked.

;(define primitive-procedures
;  (list (list 'car car)
;        (list 'cdr cdr)
;        (list 'cons cons)
;        (list 'null? null?)
;        (list '+ +)
;        (list '- -)
;        (list '* *)
;        (list '= =)
;        (list '/ /)
;        (list '> >)
;        (list '< <)
;       (list 'display display)
;        (list 'list list)
;        ))

;;; Here is where we rely on the underlying Scheme implementation to
;;; know how to apply a primitive procedure.

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


; This procedure installs the primitive procedures
; into the global environement. It has an if statement
; to check that the primitive procedure does not take the
; name of a special form. It does that by checking the
; special form table. 
(define (install-primitive-procedure name action)
  ;(display (lookup name special-table))
  (cond ((lookup name special-table)
         (display "cannot install primitive procedure with special form name"))
        ((add-binding-to-frame! name (list 'primitive action)
                                (first-frame the-global-environment))
         (display name)
         (newline))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The main driver loop
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note that (read) returns an internal representation of the next
;;; Scheme expression from the input stream.  It does NOT evaluate
;;; what is typed in -- it just parses it and returns an internal
;;; representation.  It is the job of the scheme evaluator to perform
;;; the evaluation.  In this case, our evaluator is called xeval.

; the same target as the one defined
; in the save-continuation.scm example
; It helps in recovering from errors. 
(define target '())

; Function that is invoked when there is a
; mistake. It uses the target definition above
; to go to the site of the of where target is
; set in the procedure (s450).
(define (s450error . args)

  (display "Error: ")
  (display args)
  (target args))

(define input-prompt "s450==> ") 

(define (leave)
  (newline)
  (display "exiting s450 and UMB Scheme"))

(define (s450)

  ; Continuation in which the error will recover
  (call-with-current-continuation  ; RSR5 way of call/cc
   (lambda(here)
     (set! target here)))   
  (prompt-for-input input-prompt)  
  (let ((input (read)))      
    (let ((output (xeval input the-global-environment)))
      (user-print output))

    ; checking if the input is a
    ; eof-object? or not.
    ; exit s450 if the input
    ; is an eof-object
    (if (not (eof-object? input))
        (s450)
        (leave))))
  

(define (prompt-for-input string)
  (newline) (newline) (display string))


;;; Note that we would not want to try to print a representation of the
;;; <procedure-env> below -- this would in general get us into an
;;; infinite loop.

(define (user-print object)
  (if (user-defined-procedure? object)
      (display (list 'user-defined-procedure?
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 Here we go:  define the global environment and invite the
;;;        user to run the evaluator.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define the-global-environment (setup-environment))
(define the-global-environment (xtend-environment '()
                            '()
                            '()))


; this procedure intalls the special forms.
; As long as the special form name is not
; equal to an already defined variable or is
; in the special form table, then it is inserted
; into the special form table
(define (install-special-form symbol action)
  
  (cond ((lookup symbol special-table) (display "cannot install"))
        ((defined? (list action symbol) the-global-environment)
         (display "cannot install"))
        ((insert! symbol action special-table))))

(install-special-form 'defined? (lambda (exp env) (defined? exp env)))
(install-special-form 'locally-defined?
                      (lambda (exp env) (locally-defined? exp env)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(install-special-form 'locally-make-unbound!
                      (lambda (exp env) (local-eval-unbind exp env)))

(install-special-form 'quote (lambda (exp env)(text-of-quotation exp)))
(install-special-form 'set! (lambda (exp env) (eval-assignment exp env)))
(install-special-form 'define (lambda (exp env)(eval-definition exp env)))     
(install-special-form 'if (lambda (exp env)(eval-if exp env)))

(install-special-form 'lambda (lambda (exp env)
                                (make-procedure (lambda-parameters exp)              
                                                (lambda-body exp)
                                                env)))

(install-special-form 'begin (lambda (exp env)
                               (eval-sequence (begin-actions exp) env)))

(install-special-form 'cond (lambda (exp env) (xeval (cond->if exp) env)))


; cons-stream - the first element in the stream will be evaluated
; together with the corresponding environment.
; The cdr of the stream will be packed into a thunk together
; with the remaining elements in the stream and corresponding
; environment. 
(define (cons-stream elmt strm)

  (cons (xeval (cadr elmt) strm)  (list 'thunk (caddr elmt) strm))

  )

; Instillation of cons-stream as a special form.
; The definition of cons-stream is righ above. 
(install-special-form 'cons-stream
                      (lambda (elmt strm) (cons-stream elmt strm)))                      
                        

(install-primitive-procedure 'car car)
(install-primitive-procedure 'cdr cdr)
(install-primitive-procedure 'cons cons)
(install-primitive-procedure 'null? null?)
(install-primitive-procedure '+ +)
(install-primitive-procedure '- -)
(install-primitive-procedure '* *)
(install-primitive-procedure '= =)
(install-primitive-procedure '/ /)
(install-primitive-procedure '> >)
(install-primitive-procedure '< <)
(install-primitive-procedure 'display display)
(install-primitive-procedure 'list list)
(install-primitive-procedure 'not not)
(install-primitive-procedure 'remainder remainder)
(install-primitive-procedure 'newline newline)


; installing the stream primitive procedure below
(install-primitive-procedure 'the-empty-stream '())
(install-primitive-procedure 'stream-car (lambda (strm) (car strm)))

; stream-cdr implementation: can be thought of as the evaluation
; of the rest of elements in the stream together with the
; corresponding environment.
(install-primitive-procedure 'stream-cdr (lambda (strm)
                                           (xeval (caddr strm) (cadddr strm))))
; stream-null? implemented as a simple null?
(install-primitive-procedure 'stream-null? null?)


(display "... loaded the metacircular evaluator. (s450) runs it.")
(newline)


;(s450)





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

(define (xeval exp env)
       (let ((action (lookup (type-of exp) special-table)))
        (if action
            (action exp env)
            (cond ((self-evaluating? exp) exp)
                  ((variable? exp) (lookup-variable-value exp env))
                  ((application? exp)
                   (xapply (xeval (operator exp) env)
                           (list-of-values (operands exp) env)))
                  (else
                   (error "Unknown expression type -- XEVAL " exp))))))

(define (xapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (xtend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- XAPPLY " procedure))))

;;; Handling procedure arguments

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (xeval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

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
                (error "ELSE clause isn't last -- COND->IF "
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

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

(define (compound-procedure? p)
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
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied " vars vals)
          (error "Too few arguments supplied " vars vals))))


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
        (error "Unbound variable " var)
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
        (error "Unbound variable -- SET! " var)
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

;(define (make-unbound! var env)

;  (set-variable-value! (cadr var) '() (car env))
  
; )

; do out remove-locally here. Should not be that bad. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	 The initial environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is initialization code that is executed once, when the the
;;; interpreter is invoked.

; maybe create a new version of extend-enviroment
; that will take install special form
; that would be the extreme way of doing this.
; manually create one of the inputs by using
; add-binding-to-frame! do "+" then.

;my note: the setup-environment
; is what the global environment will use. 


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

(define input-prompt "s450==> ")

(define (s450)
  ;(newline)
 ; (display the-global-environment)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (xeval input the-global-environment)))
      (user-print output)))

  (s450))

(define (prompt-for-input string)
  (newline) (newline) (display string))


;;; Note that we would not want to try to print a representation of the
;;; <procedure-env> below -- this would in general get us into an
;;; infinite loop.

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
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
;(install-primitive-procedure 'let let)
(display "... loaded the metacircular evaluator. (s450) runs it.")
(newline)

;(s450)
;(define x 4)

;(display the-global-environment)
;(lambda (exp env)(eval-if exp env))
;(install-primitive-procedure 'lambda (lambda (exp env)(eval-if exp env)))
;(install-special-form '+ +)

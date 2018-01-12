(define (tagged-list exp tag)
  (eq? (car exp) tag))

(define (self-eval? exp)
  (or (number? exp)
      (string? exp)))

(define (variable? exp)
  (symbol? exp))

(define (definition? exp)
  (tagged-list exp 'define))

(define (definition-value exp)
  (caddr exp))

(define (definition-variable exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list exp 'set!))

(define (assignment-value exp)
  (caddr exp))

(define (assignment-variable exp)
  (cadr exp))

(define (lambda? exp)
  (tagged-list exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cdr (cdr exp)))

(define (make-procedure vars bproc env)
  (list vars bproc env))

(define (procedure-body proc)
  (cadr proc))

(define (procedure-env proc)
  (caddr proc))

(define (procedure-parameters proc)
  (car proc))

(define (if? exp)
  (tagged-list exp 'if))

(define (test-statement exp)
  (cadr exp))

(define (if-true exp)
  (caddr exp))

(define (if-false exp)
  (cadddr exp))

(define (begin? exp)
  (tagged-list exp 'begin))

(define (begin-clause exp)
  (cdr exp))

(define (first-clause exp)
  (car exp))

(define (rest-clause exp)
  (cdr exp))

(define (cond? exp)
  (tagged-list exp 'cond))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (primitive-procedure? proc)
  (tagged-list proc 'primitive))

(define (amb? exp)
  (tagged-list exp 'amb))

(define (amb-choice exp)
  (cdr exp))

(define (enclosing-enviroment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define (make-frame variables values)
  (cons variables values))

(define (frame-values frame)
  (cdr frame))

(define (frame-variables frame)
  (car frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

(define (extend-enviroment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "The enviroment can NOT be made --- UNMACHED VAR VAL")))

(define (lookup-variable-value exp env)
    (define (scan vars vals)
      (cond ((null? vars) #f)
	    ((eq? (car vars) exp) (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (define (env-loop env)
      (if (null? env)
	  (error "The variable is NOT defined" exp)
	  (let ((result  (scan (frame-variables (first-frame env))
			       (frame-values (first-frame env)))))
	    (if result result (env-loop (enclosing-enviroment env))))))
    (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-enviroment env)))
	    ((eq? (car vars) var) (begin (set-car! vals val) 'ok))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (null? env) 
	(error "can NOT find the variable -- " var)
	(scan (frame-variables (first-frame env)) 
	      (frame-values (first-frame env)))))
  (env-loop env))
	  
(define (define-variable-value! var val env)
  (define (scan vars vals)
    (cond ((null? vars) (add-binding-to-frame! var val (car env)))
	  ((eq? (car vars) var) (set-car! vals val))
	  (else (scan (cdr vars) (cdr vals)))))
  (let ((first (first-frame env)))
    (scan (frame-variables first) (frame-values first))))

(define (primitive-procedure-objects)
  (map (lambda (proc)
	 (cons 'primitive (cdr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (setup-enviroment)
  (cons (make-frame (primitive-procedure-names)
		    (primitive-procedure-objects))
		    (make-frame '() '())))


(define the-global-enviroment (setup-enviroment))

(define (eval-beta exp)
  (amb-eval exp the-global-enviroment (lambda (val fail)
					(display val))
	    (lambda () '())))
(define (amb-eval exp env success fail)
  ((analyze-amb exp) env success fail))

(define (analyze-amb exp)
  (cond ((self-eval? exp) (analyze-self-eval exp))
	((variable? exp) (analyze-variable exp))
	((definition? exp) (analyze-definition exp))
	((assignment? exp) (analyze-assignment exp))
	((lambda? exp) (analyze-lambda exp))
	((if? exp) (analyze-if exp))
	((begin? exp) (analyze-sequence (begin-clause exp)))
	((amb? exp) (analyze-amb-clause exp))
	((application? exp) (analyze-application exp))
	(else (error "Unknow expression type -- EVAL" exp))))

(define (analyze-self-eval exp)
  (lambda (env success fail)
    (success exp fail)))

(define (analyze-variable exp)
  (lambda (env success fail)
    (success (lookup-variable-value exp env) fail)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(result (analyze-amb (definition-value exp))))
    (lambda (env success fail)
      (result env (lambda (value fail2)
	              (define-variable-value! var value env)
		      (success 'ok fail2))
	      fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(val (analyze-amb (assignment-value exp))))
    (lambda (env success fail)
      (val env
       (lambda (value fail)
	     (let ((origin (lookup-variable-value var env)))
	       (set-variable-value! var value env)
	       (success 'ok
			(lambda ()
			  (set-variable-value! var origin env)
			  (fail)))))
	   fail))))

(define (analyze-sequence exp)
  (define (loop a b)
    (lambda (env success fail)
      (a
       env
       (lambda (val fail2)
	 (b env success fail2))
       fail)))
  (define (make a b)
    (if (null? b)
	a
	(make (loop a (first-clause b)) (rest-clause b))))
  (let ((result (map analyze-amb exp)))
    (make (first-clause result) (rest-clause result))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (lambda-body exp))))
    (lambda (env success fail)
      (success (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
  (lambda (env success fail)
    (let ((result (analyze-amb (test-statement exp))))
      (result env (lambda (val fail2)
		    (if val
			((analyze-amb (if-true exp) env success fail2))
			((analyze-amb (if-false exp) env success fail2))))
	      fail))))

(define (analyze-application exp)
  (let ((fproc (analyze-amb (operator exp)))
	(aproc (map analyze-amb (operands exp))))
    (lambda (env success fail)
      (fproc env 
	     (lambda (proc fail1)
	       (get-args aproc env
			 (lambda (val fail2)
			   (execute-application 
			     proc val success fail2))
			 fail1))
	     fail))))

(define (get-args aproc env success fail)
  (if (null? aproc)
      (success '() fail)
      (get-args (cdr aproc) env
		(lambda (args fail2)
		  (success (cons ((car aproc) env (lambda (val fail3)
						    val) fail2)
				 args) fail2))
		fail)))

(define primitive-procedures
  (list (cons 'car car)
	(cons 'cdr cdr)
	(cons 'cons cons)
	(cons 'null? null?)
	(cons '+ +)
	(cons '- -)
	(cons '* *)
	(cons '/ /)
	(cons '> >)
	(cons '= =)
	(cons '< <)
))


(define (execute-application proc val success fail)
  (cond ((primitive-procedure? proc)
	 (success (apply (cdr proc) val) fail))
	(else (let ((variables (procedure-parameters proc))
		    (p (procedure-body proc))
		    (old-env (procedure-env proc)))
		(p (extend-enviroment variables val old-env)
		   success
		   fail)))))
		    
(define (analyze-amb-clause exp)
  (let ((result (map analyze-amb (amb-choice exp))))
    (lambda (env success fail)
      (define (iter choices)
	(if (null? choices)
	    (fail)
	    ((car choices)
	     env
	     success
	     (lambda ()
	       (iter (cdr choices))))))
      (iter result))))

(define input-prompt ";;; AMB-EVAL input:")
(define output-prompt ";;; AMB-EVAL value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
	  (try-again)
	  (begin
	    (newline) (display ";;; STARTING A NEW PROBLEM")
	    (amb-eval
	     input
	     the-global-enviroment
	     (lambda (val next)
	       (announce-output output-prompt)
	       (user-print val)
	       (internal-loop next))
	     (lambda ()
	       (announce-output
		";;; THERE ARE NO MORE VALUES OF")
	       (user-print input)
	       (dirver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


(define (user-print object)
  (display object))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))


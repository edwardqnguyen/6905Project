(define god-mode-exclusive (make-eqv-hash-table)) ; Useful for eventual eval/apply interpreter.
(define god-mode-enabled #f)

(define (function-available? func)
	(hash-table/lookup god-mode-exclusive func
			   (lambda (key) ((or (eq? (hash-table-ref key) #f)
					      (eq? god-mode-enabled #t))))
			   (lambda () (#f))))

(define (god-mode-only func)
	(lambda (arguments) (
		(if (eq? god-mode-enabled #f)
		    (tell! "God Mode Disabled")
		    (func arguments))
	)))

(define god-mode-test (god-mode-only (lambda () (tell! "its working"))))

(define (comparison trait type obj1 obj2 victor loser tie)
	(let ((val1 ((property-getter trait type) obj1))
	      (val2 ((property-getter trait type) obj2)))
	  (if (eq? val1 val2)
	    ((tie obj1)
	     (tie obj2))
	    (if (> val1 val2)
		((victor obj1)
		 (victor obj2))
		((victor obj2)
		 (victor obj1))))))

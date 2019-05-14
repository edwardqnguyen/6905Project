(define god-mode-exclusive (make-eqv-hash-table)) ; Useful for eventual eval/apply interpreter.
(define god-mode-enabled #f)

(define (enable-god-mode) (set! god-mode-enabled #t))
(define (disable-god-mode) (set! god-mode-enabled #f))

(define (function-available? func) ; Unused Sticky Note Checker for interpreter
	(hash-table/lookup god-mode-exclusive func
			   (lambda (key) ((or (eq? (hash-table-ref key) #f)
					      (eq? god-mode-enabled #t))))
			   (lambda () (#f))))

(define (god-mode-only func)
	(lambda arguments
        (if (eq? god-mode-enabled #f)
		    (narrate! (list "God-mode Disabled")
                  my-avatar)
		    (apply func arguments))))

;;;;; God-Mode Wrapped Functions

(define (god-go direction)
	((god-mode-only go) direction))

(define (god-create location)
	((god-mode-only create-place) location))

(define (god-can-go-both-ways loc1 dir1 dir2 loc2)
	((god-mode-only can-go-both-ways) loc1 dir1 dir2 loc2))

(define (god-create-people places)
	((god-mode-only create-people) places))

(define (god-create-students places)
  ((god-mode-only create-students) places))

(define (god-create-profs places)
  ((god-mode-only create-profs) places))

(define (god-create-president places)
  ((god-mode-only create-president) places))

(define (god-create-house-masters places)
  ((god-mode-only create-house-masters) places))

(define (god-create-trolls places)
  ((god-mode-only create-trolls) places))

(define (god-create-thing name location)
  ((god-mode-only create-thing) name location))

(define (god-create-mobile-thing name location)
  ((god-mode-only create-mobile-thing) name location))

(define (god-create-place name)
  ((god-mode-only create-place) name))

(define (god-create-exit from direction to)
  ((god-mode-only create-exit) from direction to))

(define (god-create-student name home restlessness acquisitiveness)
  ((god-mode-only create-student) name home restlessness acquisitiveness))

(define (god-create-house-master name home restlessness irritability)
  ((god-mode-only create-house-master) name home restlessness irritability))

(define (god-create-troll name home restlessness hunger)
  ((god-mode-only create-troll) name hoem restlessness hunger))

(define (god-create-avatar name home)
  ((god-mode-only create-avatar) name home))

(define (god-can-see a b)
  ((god-mode-only can-see) a))

(define (god-can-see-both-ways a b)
   ((god-mode-only can-see-both-ways) a b))

(define (god-can-go from direction to)
  ((god-mode-only can-go) from direction to))
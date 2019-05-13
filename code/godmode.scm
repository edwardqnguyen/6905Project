(define god-mode-exclusive (make-eqv-hash-table)) ; Useful for eventual eval/apply interpreter.
(define god-mode-enabled #f)

(define (enable-god-mode) (set! god-mode-enabled #t))
(define (disable-god-mode) (set! god-mode-enabled #f))

(define (god-mode-only func)
	(lambda arguments
        (if (eq? god-mode-enabled #f)
		    (narrate! (list "God-mode Disabled")
                  my-avatar)
		    (apply func arguments))
	))

(define (god-go direction)
	((god-mode-only go) direction))

(define (god-create location)
	((god-mode-only create-place) location))

(define (god-can-go-both-ways loc1 dir1 dir2 loc2)
	((god-mode-only can-go-both-ways) loc1 dir1 dir2 loc2))


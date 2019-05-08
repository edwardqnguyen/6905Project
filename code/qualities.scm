;Create quality properties
;We need one property for the predicate and one for the value
;This enables us to have qualities take on a variety of values
(define quality:predicate
    (make-property  'predicate predicate?
                    'default-value integer?'))

(define quality:value
    (make-property  'predicate any-object?
                    'default-value 0))

(define quality:name
    (make-property  'predicate string?
                    'default-value 'quality))

;Create quality type
(define quality?
    (make-type 'quality (list quality:predicate quality:value quality:name)))

;Property get, set, and compare operations
;Qualities should be able to take on different types but not change type once declared

(define get-quality-type
    (property-getter quality:predicate quality?))

(define get-quality-value
    (property-getter quality:value quality?))

(define (set-quality-value! object value)
    ((property-setter quality:value quality? (get-quality-type object)) object value))

;Comparison returns #t if the first argument is greater in the domain and #f if it is not.
;Once there is a response from this, we can check equality

(define compare-in-quality 
    (simple-generic-procedure 'compare-in-quality 2))

;To have a generic procedure work with qualities (and look at their internal predicates),
;we'll need a way of making special quality predicates. We'll use these as the predicates
;of the generic procedure.

(define (create-quality-predicate real-predicate?)
    (lambda (quality)
        (if (eqv? (get-quality-type quality) real-predicate?)
            #t
            #f)))

;We also need a way to add new comparison types. 
(define (add-quality-comparison-procedure! quality-type procedure)
    (let ((predicate (create-quality-predicate quality-type)))
        (define-generic-procedure-handler compare-in-quality
            (match-args predicate predicate)
            procedure)))

;Now let's add a basic one for integers
;Remember, we're trying to return true if the first argument is greater than the second

(add-quality-comparison-procedure! integer? >)
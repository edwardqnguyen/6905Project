(define (people-in-place place param)
  (filter person? (get-things place)))

(define (people-here person)
  (delv person (people-in-place (get-location person) param)))

;Creates module as a generic procedure that can be edited

;These are connection modules - they modularize connections and allow them to become editable
;By having these more customizable connections, we can use more formal (but variable) links as
;opposed to specific implementations for each caller

;Connection modules return a result modification function when passed the caller

;By providing context, we create the appropriate connector
;We provide this by providing the caller. The context is then pulled from the caller.

;We're specifically doing this for vision

(define vision-module)

(define (set-vision-module! connection-function)
    (set! vision-module connection-function)

;A connection function is merely a one-argument generic procedure

(define (create-connection-function name)
    (simple-generic-procedure name 1))

;We can add cases to our connection function. Since a connection function returns a function, we allow
;the user to specify the function that is returned. This is not the function that is run but the
;function returned by the function that is run.

(define (add-connection-function-case! connection-function case-predicate case-function)
    (define-generic-procedure-handler connection-function
        (match-args case-predicate)
        (lambda (caller) (case-function))))




;Creates module as a generic procedure that can be edited

;These are connection modules - they modularize connections and allow them to become editable
;By having these more customizable connections, we can use more formal (but variable) links as
;opposed to specific implementations for each caller

;Because connection modules don't actually get the data for us but rather process it, they can be
;thought of as a processing mechanism for received data. 

;By providing context, we create the appropriate connector
;We provide this by providing the caller. The context is then pulled from the caller.

;The actual connector here is the function (connector function) that we get from the module. The module
;is given context in the form of the caller and hands us a connection function.
;When we request information from another function, we go ahead and pass the connector function
;to that function (this encodes context) and the function we're calling does what it needs to do
;given the information.

;The connector function is a filtering function. It should take in a list of results and filter them.

;We're specifically doing this for vision

(define vision-module)

(define (set-vision-module! connection-function)
    (set! vision-module connection-function)

;A connection module is merely a one-argument generic procedure
;The single argument is the caller

(define (create-connection-function name)
    (simple-generic-procedure name 1))

;We can add cases to our connection module. Since a connection module returns a connector function, we allow
;the user to specify the function that is returned. This is not the function that is run but the
;function returned by the function that is run.

(define (add-connection-function-case! connection-function case-predicate case-function)
    (define-generic-procedure-handler connection-function
        (match-args case-predicate)
        (lambda (caller) (case-function))))

;To keep legacy programs working, we won't modify the original functions directly. However, we'll modify
;the functions we need to follow this new paradigm.

;Let's add a basic case

(add-connection-function-case! vision-module any-object? (lambda (input-list) (input-list)))

;To keep things simple, the new function calls for mod functions will always have an argument for connector-fn.
;The function can choose to ignore this if need be.

(define (mod-people-in-place place connector-fn)
  (connector-fn (filter person? (get-things place))))

(define (mod-people-here person connector-fn)
  (delv person (mod-people-in-place (get-location person) (vision-module person))))




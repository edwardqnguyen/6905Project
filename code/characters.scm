;Characters will be a modified (and generic version) of agents (people and autonomous and agents)
;They will not be a subclass of people, though, as we don't want characters
;to have health as a specific trait (we want this to be a quality)

;We've used some of the existing code for people
;Characters will have a quality bag that contains their qualities
;Characters also need to be registered so that we can update their quality bags

;People and other characters will now be sub classes of characters

(define character:quality-bag
    (make-property  'quality-bag
                    'predicate (lambda (x) (quality-bag? x)
                    'default-supplier
                    (lambda () (make-quality-bag 'name )))))

(define character?
  (make-type 'character (list character:health character:bag)))
(set-predicate<=! character? mobile-thing?)

(define get-quality-bag
    (property-getter character:quality-bag character?))

(define-generic-procedure-handler set-up! (match-args character?)
  (lambda (super character)
    (super character)
    (set-holder! (get-bag character) character)))

(define (add-quality-to-character quality)
    (add-quality! (get-quality-bag character) quality))

;Quality bags

;Quality bags are similar to containers, except we work with qualities and not moveable objects.
;We don't want to be able to move a quality from quality-bag to an actual bag (because qualities) aren't real,
;so we'll redefine similar functionality here. An extension might be to find a way to have a generic container object
;that can allow us to split between different domains (ie between the physical and abstract)

(define quality-bag:qualities
    (make-property  'qualities
                    'predicate (is-list-of quality?)
                    'default-value '())

(define quality-bag:holder
    (make-property  'holder
                    'predicate (lambda (x) (or (eqv? x #f) (character? x))
                    'default-value #f)

(define quality-bag?
  (make-type 'quality-bag (list quality-bag:qualities quality-bag:holder)))
(set-predicate<=! quality-bag? object?)

(define make-quality-bag
  (type-instantiator quality-bag?))

(define get-qualities
    (property-getter quality-bag:qualities quality-bag?))

(define add-quality!
    (property-adder quality-bag:qualities quality-bag? quality?))

(define remove-quality!
    (property-adder quality-bag:qualities quality-bag? quality?))

(define get-quality-bag-holder
  (property-getter quality-bag:holder quality-bag?))

(define set-quality-bag-holder!
  (property-setter quality-bag:holder quality-bag? character?))

(define (get-character-qualities character)
    (get-qualities (get-quality-bag character))

;Searches for quality by string
(define (get-character-quality character quality-name)
    (list-search-positive (lambda (quality) (is-quality-in-category? quality quality-name)) (get-character-qualities character)))

;Get quality values

(define (get-character-quality-value character quality-name)
    (get-quality-value (get-character-quality character quality-name)))

(define (get-character-quality-type character quality-name)
    (get-quality-type (get-character-quality character quality-name)))

(define (set-character-quality-value! character quality-name)
    (set-quality-value! (get-character-quality character quality-name)))

;Compare quality values of two characters

;Returns true if 1 is greater than 2 false if otherwise

(define (compare-characters-in-quality char-one char-two quality)
    (compare-in-quality (get-character-quality char-one quality) (get-character-quality char-two quality)))

;Now we'll define some subclasses. We will never instantiate a character directly but will
;instantiate these subclasses (subpredicates).

;Much of what you'll see below is how to person class was previously defined.
;However, here we move health related things into a quality

;Some types we can create as normal, but often we'll want a registry for the types.
;We can have a function to create registered types for us.

;Make type registered takes an existing type and registry and returns a method that allows you to
;add instances of this type to the registry

(define (make-type-registered type registry)
    (lambda (add)) (if (type add) (append! (list add))))

;We want an easy way to make new character types
;We'll package up the predicate and the registration method into one
;The package will return a list with the predicate and the registration method

(define (make-character-type type-name properties registry)
    (let ((new-character-type (make-type type-name properties)))
        (set-predicate<=! new-character-type character?)
        (let ((registration-method (make-type-registered new-character-type registry)))
            (list new-character-type registration-method)
        )))

;We'll also have methods for retrieving them
;These are trivial

(define (get-character-predicate character-bundle)
    (car character-bundle))

(define (get-character-registration-method character-bundle)
    (cadr character-bundle))

(define person:bag
  (make-property 'bag
                 'predicate (lambda (x) (bag? x))
                 'default-supplier
                 (lambda () (make-bag 'name 'my-bag))))

(define person-registry (list))

(define person-bundle make-character-type 'person (list person:bag) person-registry)

(define person? (get-character-predicate person-bundle))

(define register-person! (get-character-registration-method person-bundle))

(define get-bag
  (property-getter person:bag person?))

;Here is where we set up a person. When we do this, we need to register the person
;We also need to set up health for the person

(define-generic-procedure-handler set-up! (match-args person?)
  (lambda (super person)
    (super person)
    (register-person! person)
    (set-holder! (get-bag person) person)
    (let ((health (make-quality 'health int? 100)))
        (add-quality-to-character health))))

;We now redefine the health methods to affect the health quality

(define (get-health person)
    (get-character-quality-value person 'heatlh))

(define (set-health person)
    (set-character-quality-value! person 'health))


(define-generic-procedure-handler get-things (match-args person?)
  (lambda (person)
    (get-things (get-bag person))))

(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (narrate! (list person "enters" (get-location person))
              person)
    (let ((people (people-here person)))
      (if (n:pair? people)
          (say! person (cons "Hi" people))))))

(define (when-alive callback)
  (lambda (person)
    (if (n:> (get-health person) 0)
        (callback person))))

(define (people-here person)
  (delv person (people-in-place (get-location person))))

(define (things-here person)
  (things-in-place (get-location person)))

(define (vistas-here person)
  (get-vistas (get-location person)))

(define (exits-here person)
  (get-exits (get-location person)))

(define (peoples-things person)
  (append-map get-things (people-here person)))

(define (suffer! hits person)
  (guarantee n:exact-positive-integer? hits)
  (say! person (list "Ouch!" hits "hits is more than I want!"))
  (set-health! person (- (get-health person) hits))
  (if (< (get-health person) 1)
      (die! person)))

(define (die! person)
  (for-each (lambda (thing)
              (drop-thing! thing person))
            (get-things person))
  (announce!
   '("An earth-shattering, soul-piercing scream is heard..."))
  (set-health! person 0)
  (move! person (get-heaven) person))

(define (resurrect! person health)
  (guarantee n:exact-positive-integer? health)
  (set-health! person health)
  (move! person (get-origin person) person))

  
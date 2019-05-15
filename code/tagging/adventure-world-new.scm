(define the-clock)
(define all-places)
(define heaven)
(define all-people)
(define my-avatar)

(define (start-adventure my-name)
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name (random-choice all-places)))
  (whats-here))

(define (get-all-places)
  all-places)

(define (get-heaven)
  heaven)

(define (get-clock)
  the-clock)

;;; User interface

(define (go direction)
  (let ((exit
         (find-exit-in-direction direction
                                 (get-location my-avatar))))
    (if exit
        (take-exit! exit my-avatar)
        (narrate! (list "No exit in" direction "direction")
                  my-avatar)))
  unspecific)

(define (take-thing name)
  (let ((thing (find-thing name (here))))
    (if thing
        (take-thing! thing my-avatar)))
  unspecific)

(define (drop-thing name)
  (let ((thing (find-thing name my-avatar)))
    (if thing
        (drop-thing! thing my-avatar)))
  unspecific)

(define (look-in-bag #!optional person-name)
  (let ((person
         (if (default-object? person-name)
             my-avatar
             (find-person person-name))))
    (if person
        (tell! (let ((referent (local-possessive person))
                     (things (get-things person)))
                 (if (n:pair? things)
                     (cons* referent "bag contains" things)
                     (list referent "bag is empty")))
               my-avatar)))
  unspecific)

(define (whats-here)
  (look-around my-avatar)
  unspecific)

(define (say . message)
  (say! my-avatar message)
  unspecific)

(define (tell person-name . message)
  (tell! message (find-person person-name))
  unspecific)

(define (hang-out ticks)
  (do ((i 0 (n:+ i 1)))
      ((not (n:< i ticks)))
    (tick! (get-clock)))
  unspecific)

;;; Support for UI

(define (here)
  (get-location my-avatar))

(define (find-person name)
  (let ((person
         (find-object-by-name name (people-here my-avatar))))
    (if (not person)
        (tell! (list "There is no one called" name "here")
               my-avatar))
    person))

(define (find-thing name person-or-place)
  (let ((thing
         (find-object-by-name
          name
          (person-or-place-things person-or-place))))
    (if (not thing)
        (tell! (cons* "There is nothing called"
                      name
                      (person-or-place-name person-or-place))
               my-avatar))
    thing))

(define (person-or-place-things person-or-place)
  (if (place? person-or-place)
      (all-things-in-place person-or-place)
      (get-things person-or-place)))

(define (person-or-place-name person-or-place)
  (if (place? person-or-place)
      '("here")
      (list "in" (local-possessive person-or-place) "bag")))

(define (local-possessive person)
  (if (eqv? person my-avatar)
      "Your"
      (possessive person)))

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

(define (can-go from direction to)
  (create-exit from direction to))

(define (can-see a b)
  (add-vista! a b))

(define (can-see-both-ways a b)
  (can-see a b)
  (can-see b a))



(define (create-mit)
  (let ((bldg-1 (create-place 'bldg-1))
        (bldg-5 (create-place 'bldg-5))
        (bldg-3 (create-place 'bldg-3))
        (bldg-7 (create-place 'bldg-7))
        (bldg-10 (create-place 'bldg-10))
        (bldg-4 (create-place 'bldg-4))
        (bldg-2 (create-place 'bldg-2))
        (bldg-6 (create-place 'bldg-6))
        (bldg-8 (create-place 'bldg-8))

        (elevator (create-place 'elevator))
        (ground (create-place 'ground))

    (can-go-both-ways bldg-1 'north 'south bldg-5)
    (can-go-both-ways bldg-1 'ladder 'ladder bldg-3)
    (can-go-both-ways bldg-5 'north 'south bldg-7)

    (can-go-both-ways bldg-7 'east 'west bldg-3)
    (can-go-both-ways bldg-3 'east 'west bldg-10)
    (can-go-both-ways bldg-10 'east 'west bldg-4)
    (can-go-both-ways bldg-4 'ledgehug 'ledgehug bldg-8)

    (can-go-both-ways bldg-2 'north 'south bldg-6)
    (can-go-both-ways bldg-2 'north 'south bldg-4)
    (can-go-both-ways bldg-6 'ledgehug 'ledgehug bldg-8)

    (can-go-both-ways ground 'into 'outof elevator)
    (can-go-both-ways elevator 'elevate 'descend bldg-3)

    (can-go-both-ways bldg-1 'secret-bridge 'secret-bridge bldg-2)

    (can-go bldg-10 'hangdrop bldg-7)
    (can-go bldg-10 'hangdrop bldg-8)
    
    (can-go bldg-1 'down ground)
    (can-go bldg-2 'down ground)
    (can-go bldg-3 'down ground)
    (can-go bldg-4 'down ground)
    (can-go bldg-5 'down ground)
    (can-go bldg-6 'down ground)
    (can-go bldg-7 'down ground)
    (can-go bldg-8 'down ground)
    (can-go bldg-10 'down ground)

    ; Add line-of-sight into the mix
    (can-see-both-ways bldg-1 bldg-3)
    (can-see-both-ways bldg-1 bldg-5)
    (can-see-both-ways bldg-1 bldg-4)
    (can-see-both-ways bldg-1 bldg-2)
    (can-see-both-ways bldg-1 bldg-10)

    (can-see-both-ways bldg-3 bldg-5)
    (can-see-both-ways bldg-3 bldg-7)
    (can-see-both-ways bldg-3 bldg-2)
    (can-see-both-ways bldg-3 bldg-4)
    (can-see-both-ways bldg-3 bldg-10)

    (can-see-both-ways bldg-7 bldg-5)
    (can-see-both-ways bldg-7 bldg-10)

    (can-see-both-ways bldg-5 bldg-10)

    (can-see-both-ways bldg-4 bldg-10)
    (can-see-both-ways bldg-4 bldg-8)
    (can-see-both-ways bldg-4 bldg-6)
    (can-see-both-ways bldg-4 bldg-2)

    (can-see-both-ways bldg-8 bldg-6)
    (can-see-both-ways bldg-8 bldg-10)

    (can-see-both-ways bldg-6 bldg-2)
    (can-see-both-ways bldg-6 bldg-10)

    (can-see-both-ways bldg-2 bldg-10)



    (can-see bldg-1 ground)
    (can-see bldg-2 ground)
    (can-see bldg-3 ground)
    (can-see bldg-4 ground)
    (can-see bldg-5 ground)
    (can-see bldg-6 ground)
    (can-see bldg-7 ground)
    (can-see bldg-8 ground)
    (can-see bldg-10 ground)

    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-thing 'calder-sculpture the-dot)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)

    (list bldg-1 bldg-2 
        bldg-3 bldg-4 bldg-5 
        bldg-6 bldg-7 bldg-8 
        bldg-10 ground elevator)))



(define (create-people places)
  (append (create-students places)
          ;;(create-profs places)
          ;;(create-president places)
          (create-house-masters places)
          (create-trolls places)))

(define (create-students places)
  (map (lambda (name)
         (create-student name
                         (random-choice places)
                         (random-bias 5)
                         (random-bias 5)))
       '(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

(define (create-profs places)
  (map (lambda (name)
         (create-professor name
                           (random-choice places)
                           1/3
                           1/3))
       '(rob-miller eric-grimson)))

(define (create-president places)
  (create-president 'rafael-reif
                    (random-choice places)
                    (random-bias 3)
                    (random-bias 3)))

(define (create-house-masters places)
  (map (lambda (name)
         (create-house-master name
                              (random-choice places)
                              (random-bias 3)
                              (random-bias 3)))
       '(dr-evil mr-bigglesworth)))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       (random-bias 3)))
       '(grendel registrar)))

(define (create-thing name location)
  (make-thing 'name name
              'location location))

(define (create-mobile-thing name location)
  (make-mobile-thing 'name name
                     'location location))

(define (create-place name)
  (make-place 'name name))

(define (create-exit from direction to)
  (make-exit 'name 'exit
             'from from
             'direction direction
             'to to))

(define (create-student name home restlessness acquisitiveness)
  (make-student 'name name
                'location home
                'quality-bag (make-quality-bag '() #f)
                'restlessness restlessness
                'acquisitiveness acquisitiveness))

(define (create-house-master name home restlessness irritability)
  (make-house-master 'name name
                     'location home
                     'quality-bag (make-quality-bag '() #f)
                     'restlessness restlessness
                     'acquisitiveness 1/10
                     'irritability irritability))

(define (create-troll name home restlessness hunger)
  (make-troll 'name name
              'location home
              'quality-bag (make-quality-bag '() #f)
              'restlessness restlessness
              'acquisitiveness 1/10
              'hunger hunger))

(define (create-avatar name home)
  (make-avatar 'name name
               'location home
               'quality-bag (make-quality-bag '() #f)
               'screen (make-screen 'name 'the-screen)))


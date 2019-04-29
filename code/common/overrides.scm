(let-syntax
    ((define-override
       (er-macro-transformer
        (lambda (form rename compare)
          `(,(rename 'define)
            ,(symbol 'n: (cadr form))
            (,(rename 'access)
             ,(cadr form)
             ,(rename 'system-global-environment)))))))
  (define-override *)
  (define-override +)
  (define-override -)
  (define-override /)
  (define-override <)
  (define-override <=)
  (define-override =)
  (define-override >)
  (define-override >=)
  (define-override abs)
  (define-override acos)
  (define-override angle)
  (define-override asin)
  (define-override atan)
  (define-override boolean?)
  (define-override ceiling)
  (define-override cell?)
  (define-override complex?)
  (define-override cos)
  (define-override exact-integer?)
  (define-override exact-nonnegative-integer?)
  (define-override exact-positive-integer?)
  (define-override exact-rational?)
  (define-override exp)
  (define-override expt)
  (define-override floor)
  (define-override imag-part)
  (define-override integer?)
  (define-override list?)
  (define-override log)
  (define-override magnitude)
  (define-override make-cell)
  (define-override make-polar)
  (define-override make-rectangular)
  (define-override max)
  (define-override min)
  (define-override negative?)
  (define-override null?)
  (define-override number?)
  (define-override pair?)
  (define-override positive?)
  (define-override pp)
  (define-override pretty-print)
  (define-override procedure?)
  (define-override rational?)
  (define-override real-part)
  (define-override real?)
  (define-override remainder)
  (define-override round)
  (define-override sin)
  (define-override sqrt)
  (define-override square)
  (define-override string?)
  (define-override symbol?)
  (define-override tan)
  (define-override truncate)
  (define-override vector?)
  (define-override zero?))
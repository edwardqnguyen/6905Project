(let ((env (make-top-level-environment)))
  (with-working-directory-pathname
   (directory-pathname (current-load-pathname))
   (lambda ()
     (load
      '("common/overrides"                 "common/utils"
        "common/indexes"                   "common/collections"
        "common/memoizers"                 "common/predicates"
        "common/applicability"             "common/generic-procedures"
        "common/operators"                 "common/package"
        "common/predicate-counter"         "common/trie"
        "tagging/generics"                 "tagging/tagging"
        "tagging/predicates"               "tagging/templates"
        "tagging/values"                   "tagging/tags"
        "tagging/functions"                "tagging/operations"
        "common/arith"                     "common/numeric-arith"
        "generic-arithmetic/generic-arith" "tagging/standard-arith"
        "tagging/vector-arith"             "tagging/adventure-substrate"
        "tagging/adventure-objects"        "tagging/adventure-world")
      env)))
  (environment-define system-global-environment 'current-book-environment env)
  (ge env))

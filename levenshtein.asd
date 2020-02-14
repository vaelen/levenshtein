(defsystem "levenshtein"
  :components ((:file "levenshtein")))

(defsystem "levenshtein/executable"
    :build-operation program-op
    :build-pathname "levenshtein" ;; shell name
    :entry-point "levenshtein::main" ;; thunk
    :components ((:file "levenshtein")))

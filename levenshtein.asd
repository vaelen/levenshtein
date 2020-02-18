(defsystem "levenshtein"
  :components ((:file "levenshtein"))
  :in-order-to ((test-op (test-op "levenshtein/tests"))))

(defsystem "levenshtein/executable"
    :build-operation program-op
    :build-pathname "levenshtein" ;; shell name
    :entry-point "levenshtein::main" ;; thunk
    :components ((:file "levenshtein")))

(defsystem "levenshtein/tests"
  :depends-on ("levenshtein")
  :components ((:file "levenshtein-tests"))
  :perform (test-op (o c) (uiop:symbol-call :levenshtein-tests '#:run)))

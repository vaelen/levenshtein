LISP=sbcl

all: build

build:
	$(LISP) --eval '(require "asdf")' --eval '(asdf:make :levenshtein/executable)' --quit

clean:
	rm levenshtein

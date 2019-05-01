(defsystem "cl-lemma"
  :version "0.1.0"
  :author "t-cool"
  :license "MIT"
  :depends-on (cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "a lemmatizer implemented in Common Lisp"
  :in-order-to ((test-op (test-op "cl-lemma/tests"))))

(defsystem "cl-lemma/tests"
  :author "t-cool"
  :license "MIT"
  :depends-on ("cl-lemma"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-lemma"

  :perform (test-op (op c) (symbol-call :rove :run c)))

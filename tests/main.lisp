(defpackage cl-lemma/tests/main
  (:use :cl
        :cl-lemma
        :rove))
(in-package cl-lemma/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lemmatizer)' in your Lisp.

(deftest lemmatize
  (testing "noun"
	   (ok (equal (cl-lemma:lemma "leaves" :noun) "leaf"))
	   (ok (equal (cl-lemma:lemma "libraries" :noun) "library"))
	   )
  (testing "verb"
	   (ok (equal (cl-lemma:lemma "leaves" :verb) "leave"))
	   (ok (equal (cl-lemma:lemma "went" :verb) "go"))
	   )
  (testing "adjective"
	   (ok (equal (cl-lemma:lemma "better" :adj) "good"))
	   )
  (testing "adverb"
	   (ok (equal (cl-lemma:lemma "better" :adv) "well"))
	   )
)
  
	       

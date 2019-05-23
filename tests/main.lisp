(defpackage cl-lemma/tests/main
  (:use :cl
        :cl-lemma
        :rove))
(in-package cl-lemma/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-lemma)' in your Lisp.

(deftest lemmatize
  (testing "noun"
	   (ok (equal (cl-lemma:lemma "leaves" :noun) "leaf"))
	   (ok (equal (cl-lemma:lemma "libraries" :noun) "library"))
	   )
  (testing "verb"
	   (ok (equal (cl-lemma:lemma "leaves" :verb) "leave"))
	   (ok (equal (cl-lemma:lemma "went" :verb) "go"))
	   (ok (equal (cl-lemma:lemma "abbreviated" :verb) "abbreviate"))
	   (ok (equal (cl-lemma:lemma "arisen" :verb) "arise"))
	   (ok (equal (cl-lemma:lemma "bound" :verb) "bind"))	   
	   )
  (testing "adjective"
	   (ok (equal (cl-lemma:lemma "better" :adj) "good"))
	   )
  (testing "adverb"
	   (ok (equal (cl-lemma:lemma "better" :adv) "well"))
	   )
  (testing "general"
	   (ok (equal (cl-lemma:lemma "studied") "study"))
	   (ok (equal (cl-lemma:lemma "abbreviated") "abbreviate"))    		   )  
)
  
	       

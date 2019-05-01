(defpackage cl-lemma
  (:use :cl)
  (:import-from :cl-ppcre
		:scan)
  (:export :lemma
	   :load-dic
	   :*rules*))
(in-package :cl-lemma)

(defmacro load-dic(name path)
  `(defvar ,name 
     (loop
	for line in (uiop:read-file-lines
		     (merge-pathnames ,path
			            (asdf:system-source-directory "cl-lemma")))
	unless (eql (aref line 0) #\#)
	collect (uiop:split-string line :separator '(#\Tab)))))

(load-dic *exc-adj* "dict/exc_adj.tsv")
(load-dic *exc-adv* "dict/exc_adv.tsv")
(load-dic *exc-noun* "dict/exc_noun.tsv")
(load-dic *exc-verb* "dict/exc_verb.tsv")
(load-dic *index-adv* "dict/index_adv.tsv")
(load-dic *index-noun* "dict/index_noun.tsv")
(load-dic *index-verb* "dict/index_verb.tsv")

(defvar *rules*
'((noun
   (("s" . "" )("ses" . "s")("ves" . "f")
    ("xes" . "x")("zes" . "z")("ches" . "ch")
    ("shes" . "sh")("men" . "man")("ies" . "y")))
  (verb
   (("s" . "" )("ies" . "y")("es" . "e")
    ("es" . "")("ed" . "e")("ed" . "")
    ("ing" . "e")("ing" . "" )))
  (adj
   (("er" . "")("est" . "")("er" . "e")("est" . "e")))
  (adv
   (("er" . "")("est" . "")("er" . "e")("est" . "e")))))

(defun lemma()
  (ppcre:scan "ses" "buses"))

;; (loop for i in (cadr (assoc 'noun cl-lemma:*rules*)) do (print i))
;; ("s" . "") 
;; ("ses" . "s") 
;; ("ves" . "f") 
;; ("xes" . "x") 
;; ("zes" . "z") 
;; ("ches" . "ch") 
;; ("shes" . "sh") 
;; ("men" . "man") 
;; ("ies" . "y")

;; cl-lemma::*exc-adv*
;; (("best" "well") ("better" "well") ("deeper" "deeply") ("farther" "far") ("further" "far") ("harder" "hard") ("hardest" "hard"))

;; (ppcre:scan "s" "looks")
;; 4
;; 5

(defpackage cl-lemma
  (:use :cl)
  (:export :lemma
	   :load-dic
	   :*rules*))
(in-package :cl-lemma)

(defmacro load-dic(path)
  `(loop
      for line in (uiop:read-file-lines
		   (merge-pathnames ,path
			            (asdf:system-source-directory "cl-lemma")))
      unless (eql (aref line 0) #\#)
      collect (uiop:split-string line :separator '(#\Tab))))

(load-dic "dict/exc_adj.tsv")
(load-dic "dict/exc_adv.tsv")
(load-dic "dict/exc_noun.tsv")
(load-dic "dict/exc_verb.tsv")
(load-dic "dict/index_adv.tsv")
(load-dic "dict/index_noun.tsv")
(load-dic "dict/index_verb.tsv")

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

(defun lemma(word pos)
  (print "not yet"))

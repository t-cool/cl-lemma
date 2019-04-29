(defpackage cl-lemma
  (:use :cl)
  (:export :lemma))
(in-package :cl-lemma)


(defvar *rules* nil)
(defvar *exception* nil)

(defun lemma(word pos)
  (flet((original-form(word)
	  word))
  (let((result word))
    (original-form word)))

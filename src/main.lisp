(defpackage cl-lemma
  (:use :cl)
  (:import-from :cl-ppcre
		:scan
		:regex-replace)
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

(load-dic |*exc-adj*| "dict/exc_adj.tsv")
(load-dic |*exc-adv*| "dict/exc_adv.tsv")
(load-dic |*exc-noun*| "dict/exc_noun.tsv")
(load-dic |*exc-verb*| "dict/exc_verb.tsv")
(load-dic |*index-adv*| "dict/index_adv.tsv")
(load-dic |*index-noun*| "dict/index_noun.tsv")
(load-dic |*index-verb*| "dict/index_verb.tsv")

(defmacro find-exc-of-pos (word pos)
  `(loop
      for pair in ,(intern (concatenate 'string "*exc-" pos "*"))
      if (string-equal ,word (car pair))
      return (cadr pair)))

; (macroexpand-1 '(find-exc-of-pos "better" "adj"))
; (find-exc-of-pos "better" "adv")
; (find-exc-of-pos "barmiest" "adj")
; (find-exc-of-pos "abaci" "noun")
; (find-exc-of-pos "alkalified" "verb")

(defmacro find-index-of-pos (word pos)
  `(loop
      for pair in ,(intern (concatenate 'string "*index-" pos "*"))
      if (string-equal ,word (car pair))
      return t))

; (find-index-of-pos "above" "adv")
; (find-index-of-pos "abashment" "noun")
; (find-index-of-pos "ablate" "verb")

(defun lemma-noun(word)
    (cond
      ((ppcre:scan "ses$" word)(ppcre:regex-replace "ses$" word  "s"))
      ((ppcre:scan "ves$" word)(ppcre:regex-replace "ves$" word  "f"))
      ((ppcre:scan "xes$" word)(ppcre:regex-replace "xes$" word  "x"))
      ((ppcre:scan "zes$" word)(ppcre:regex-replace "zes$" word  "z"))
      ((ppcre:scan "ches$" word)(ppcre:regex-replace "ches$" word  "ch"))
      ((ppcre:scan "shes$" word)(ppcre:regex-replace "shes$" word  "sh"))
      ((ppcre:scan "men$" word)(ppcre:regex-replace "men$" word  "man"))
      ((ppcre:scan "ies$" word)(ppcre:regex-replace "ies$" word  "y"))
      ((ppcre:scan "s$" word)(ppcre:regex-replace "s$" word  ""))))

(defun lemma-verb(word)
    (if (find-word word)
	(cond
	  ((ppcre:scan "es$" word)(ppcre:regex-replace "es$" word  "e"))
	  ((ppcre:scan "ed$" word)(ppcre:regex-replace "ed$" word  "e"))
	  ((ppcre:scan "ing$" word)(ppcre:regex-replace "ing$" word  "e")))
	(cond 
	  ((ppcre:scan "ies$" word)(ppcre:regex-replace "ies$" word  "y"))
	  ((ppcre:scan "es$" word)(ppcre:regex-replace "es$" word  ""))
	  ((ppcre:scan "ed$" word)(ppcre:regex-replace "ed$" word  ""))
	  ((ppcre:scan "ing$" word)(ppcre:regex-replace "ing$" word  ""))
	  ((ppcre:scan "s$" word)(ppcre:regex-replace "s$" word  "")))))

(defun lemma-adj(word)
    (if (find-word word)
	(cond
	  ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  "e"))
	  ((ppcre:scan "est$" word)(ppcre:regex-replace "ed$" word  "e"))) 
	(cond 
	  ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  ""))
	  ((ppcre:scan "est$" word)(ppcre:regex-replace "ed$" word  "")))))

(defun lemma-adv(word)
    (if (find-word word)
	(cond
	  ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  "e"))
	  ((ppcre:scan "est$" word)(ppcre:regex-replace "ed$" word  "e"))) 
	(cond 
	  ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  ""))
	  ((ppcre:scan "est$" word)(ppcre:regex-replace "ed$" word  "")))))

; (lemma-noun "buses")

(defun lemma(word)
  (cond
    ((find-exc-of-pos word "adv") (find-exc-of-pos word "adv"))
    ((find-exc-of-pos word "adj") (find-exc-of-pos word "adj"))
    ((find-exc-of-pos word "noun") (find-exc-of-pos word "noun"))
    ((find-exc-of-pos word "verb") (find-exc-of-pos word "verb"))
    (t
     (cond
       ((find-index-of-pos word "adv")(lemma-adv word))
       ((find-index-of-pos word "noun")(lemma-noun word))
       ((find-index-of-pos word "verb")(lemma-verb word))
       (t word)))))

; (lemma "went")
; (lemma "gone")
; (lemma "best")

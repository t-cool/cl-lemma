(ql:quickload :cl-ppcre)

(defpackage cl-lemma
  (:use :cl)
  (:import-from :cl-ppcre
		:scan
		:regex-replace)
  (:export :lemma))
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

(load-dic |*index-adj*| "dict/index_adj.tsv")
(load-dic |*index-adv*| "dict/index_adv.tsv")
(load-dic |*index-noun*| "dict/index_noun.tsv")
(load-dic |*index-verb*| "dict/index_verb.tsv")

(defmacro find-exc-of-pos (word pos)
  `(loop
      for pair in ,(intern (concatenate 'string "*exc-" pos "*"))
      if (string-equal ,word (car pair))
      return (cadr pair)))

(defmacro find-index-of-pos (word pos)
  `(loop
      for pair in ,(intern (concatenate 'string "*index-" pos "*"))
      if (string-equal ,word (car pair))
      return (car pair)))

(defun lemma-general(word)
  (cond
    ((ppcre:scan "ses$" word)(ppcre:regex-replace "ses$" word  "s"))
    ((ppcre:scan "ves$" word)(ppcre:regex-replace "ves$" word  "f"))
    ((ppcre:scan "xes$" word)(ppcre:regex-replace "xes$" word  "x"))
    ((ppcre:scan "zes$" word)(ppcre:regex-replace "zes$" word  "z"))
    ((ppcre:scan "ches$" word)(ppcre:regex-replace "ches$" word  "ch"))
    ((ppcre:scan "shes$" word)(ppcre:regex-replace "shes$" word  "sh"))
    ((ppcre:scan "men$" word)(ppcre:regex-replace "men$" word  "man"))
    ((ppcre:scan "ies$" word)(ppcre:regex-replace "ies$" word  "y"))
    ((ppcre:scan "es$" word)(ppcre:regex-replace "es$" word  "e"))
    ((ppcre:scan "ed$" word)(ppcre:regex-replace "ed$" word  ""))
    ((ppcre:scan "ing$" word)(ppcre:regex-replace "ing$" word  ""))
    ((ppcre:scan "s$" word)(ppcre:regex-replace "s$" word  ""))))

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
  (cond 
    ((ppcre:scan "ies$" word)(ppcre:regex-replace "ies$" word  "y"))
    ((ppcre:scan "es$" word)(ppcre:regex-replace "es$" word  ""))
    ((ppcre:scan "ed$" word)(ppcre:regex-replace "ed$" word  ""))
    ((ppcre:scan "ing$" word)(ppcre:regex-replace "ing$" word  ""))
    ((ppcre:scan "s$" word)(ppcre:regex-replace "s$" word  ""))))

(defun lemma-verb-special(word)
  (cond 
    ((ppcre:scan "ies$" word)(ppcre:regex-replace "ies$" word  "y"))
    ((ppcre:scan "es$" word)(ppcre:regex-replace "es$" word  "e"))
    ((ppcre:scan "ed$" word)(ppcre:regex-replace "ed$" word  "e"))
    ((ppcre:scan "ing$" word)(ppcre:regex-replace "ing$" word  ""))
    ((ppcre:scan "s$" word)(ppcre:regex-replace "s$" word  ""))))

(defun delete-last-char(word)
  (let* ((len (length word)))
    (subseq word 0 (1- len))))

(defun lemma-adj(word)
  (cond 
    ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  ""))
    ((ppcre:scan "est$" word)(ppcre:regex-replace "est$" word  ""))))

(defun lemma-adj-special(word)
  (cond 
    ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  "e"))
    ((ppcre:scan "est$" word)(ppcre:regex-replace "est$" word  "e"))))

(defun lemma-adv(word)
  (cond 
    ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  ""))
    ((ppcre:scan "est$" word)(ppcre:regex-replace "est$" word  ""))))

(defun lemma-adv-special(word)
  (cond 
    ((ppcre:scan "er$" word)(ppcre:regex-replace "er$" word  "e"))
    ((ppcre:scan "est$" word)(ppcre:regex-replace "est$" word  "e"))))
  
(defun pre-lemma(word &optional (pos nil))
  ;; if the pos is not specified, it is determined in order
  (if(null pos)
     (cond
       ((find-exc-of-pos word "noun")(find-exc-of-pos word "noun"))
       ((find-exc-of-pos word "verb")(find-exc-of-pos word "verb"))
       ((find-exc-of-pos word "adj")(find-exc-of-pos word "adj"))
       ((find-exc-of-pos word "adv")(find-exc-of-pos word "adv"))
       ;; if a lemma ends with e       
       ((find-index-of-pos (delete-last-char word) "verb")
	(lemma-verb-special word))
       ((lemma-general word)(lemma-general word))
       (t word))
     ;; if the pos is specified
     (cond
	  ((string-equal pos "noun")
	     (if (find-exc-of-pos word "noun")
		 (find-exc-of-pos word "noun")
		 (if (find-index-of-pos (delete-last-char word) "noun")
		     (lemma-noun word)
		     (lemma-noun word))))
	  ((string-equal pos "verb")
	   (if (find-exc-of-pos word "verb")
	       (find-exc-of-pos word "verb")
	       (if (find-index-of-pos (delete-last-char word) "verb")
		   (lemma-verb-special word)
		   (lemma-verb word))))
	  ((string-equal pos "adv")
	     (if (find-exc-of-pos word "adv")
	  	 (find-exc-of-pos word "adv")
	  	 (if (find-index-of-pos word "adv")
		     (lemma-adv word)
	  	     (if (lemma-adv-special word)
	  		 (lemma-adv-special word)
	  		 word))))
	  ((string-equal pos "adj")
	     (if (find-exc-of-pos word "adj")
		 (find-exc-of-pos word "adj")
		 (if (find-index-of-pos word "adj")
		     (lemma-adj word)
		     (lemma-adj-special word))))
	  ((lemma-general word)(lemma-general word))
	  (t word))))

(defun lemma (word &optional (pos nil))
  (if (pre-lemma word pos)
      (pre-lemma word pos)
      word))

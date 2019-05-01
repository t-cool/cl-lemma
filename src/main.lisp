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
   (("ses" . "s")("ves" . "f")("xes" . "x")
    ("zes" . "z")("ches" . "ch")("shes" . "sh")
    ("men" . "man")("ies" . "y")("s" . "" )))
  (verb
   (("ies" . "y")("es" . "e")
    ("es" . "")("ed" . "e")("ed" . "")
    ("ing" . "e")("ing" . "" )("s" . "" )))
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

#|
辞書の作成
・@wordlistsと@exceptions、2つのハッシュを辞書として作成。
・それぞれ、次のような構造。これらにデータを登録する。
{:noun=>{}, :verb=>{}, :adj=>{}, :adv=>{}, :abbr=>{}, :unknown=>{}}
・wordlistsに見出し語を登録する目的は、品詞を特定せずにlemmaを呼び出した際、どの品詞に属しているかを特定するため。

lemmaメソッドの呼び出し
<品詞を特定する場合>
・@exceptionsに単語があれば、そのデータを元に原形を返す
・なければ、morphological substitution (形態論の置き換え)のルールにしたがって置き換える。

<品詞を特定しない場合>
・動詞->名詞->形容詞->副詞の順で[:verb, :noun, :adj, :adv, :abbr]、見出し語から品詞を特定する。
・@exceptionsに単語があれば、そこから原形を返す
・動詞で見出し語がeで終わっていて、検索する語がedで終わっている場合、
　edを取らずにdだけをとる。
・なければ、morphological substitution (形態論の置き換え)のルールにしたがって置き換える。

#|
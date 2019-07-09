;;; Their is a total of 256 sentences possible from this grammar b/c every sentence
;;; in this language hs the form Article-Noun-Verb-Article-Noun so that means:
;;; (2 x 4 x 4 x 2 x 4) = 256 possiblities.
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

;;; This shows the robustness of the current program and how it is easily expandable
;;; with its current design. The current design involves an additional step in
;;; comparison to the straight forward method, however larger programs as well as
;;; programs that need expandability in LISP use this sort of building mechanism.
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

;;; A grammar variable that can be changed within the environment.
(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

(defun mappend (fn x)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn x)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Returnsa list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generates a random sentence or phrase."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

;;; NOTE:: This function does not fair well with recursive grammars such as the rule
;;; 'Adj* -> Adj + Adj*' that appears in *bigger-grammar* since it leads to an infinite
;;; amount of possiblities from that grammar, only test using the original *simple-grammar*
(defun generate-all (phrase)
  "Generates a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2))) -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
                (mapcar #'(lambda (x)
                              (append x y)) xlist)) ylist))

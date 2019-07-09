;;; Functions necessary for creating Exercise 2.1 & Exercise 2.2
;;; These functions exist within the englishcfgrulebase.lisp example.
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

(defun mappend (fn x)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn x)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Returns a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

;;; Exercise 2.1
;;; Medium
;;; Write a version of generate that uses cond but avoids calling
;;; rewrites twice.
(defun generate (phrase)
  "A version of generate that uses cond and avoids calling rewrite twice"
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((not (null choices))
           (generate (random-elt choices)))
          (t (list phrase)))))

;;; Exercise 2.2
;;; Medium
;;; Write a version of generate that explicitly differentiates between
;;; terminal symbols (those with no rewrite rules) and nonterminal symbols.
(defun generate-explicit (phrase)
  "A version of genrate that explicitly differentiates b/w terminal and nonterminals."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun non-terminal-p (category)
  "True if this is a category in the grammar."
  (not (null (rewrites category))))

;;; Exercise 2.3
;;; Hard
;;; Write a trivial grammar for some other language. This can be a natural language
;;; other than English, or perhaps a subset of a computer language.
;;; NOTE: This is not a perfect grammar, however it is unambigious (a cheap way of doing so)
;;; but this gives testers an idea of how C code could be wrapped within a CLISP interpreted
;;; environment. To test, before calling generate, call: (setf *grammar* *c-subset*)
;;; NOTE 2: You can import the generate-all function from englishcfgrulebase.lisp
;;;to see the maximum of total lines that could be produced...)
(defparameter *c-subset*
  '((line -> (if_statement) (assignment_statement))
    (if_statement -> (statement1 conditional statement2))
    (assignment_statement -> (assign = assigne))
    (conditional -> < > <= >= == !=)
    (statement1 -> (variable1))
    (statement2 -> (variable2))
    (assign -> (variable1))
    (assigne -> (variable2))
    (variable1 -> x y z)
    (variable2 -> a b c)))

;;; Exercise 2.4
;;; Medium
;;; One way of describing combine-all is taht it calculates the cross-product of
;;; the function append on the argument lists. Write a higher-order function
;;; cross-product, anddefine combine-all in terms of it. The moral is to make
;;; your code as general as possible, because you never know what you may
;;; want to do with it next...
(defun cross-product (fn x y)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
                (mapcar #'(lambda (x)
                            (funcall fn x y)) x)) y))

(defun combine-all (x y)
  "Return a list of lists formed by appending a y to an x."
  (cross-product #'append x y))

;;; french-english dictionary example
;;;(setf dict '((one un) (two deux) (three trois) (four quatre) (five cinq)))
;;; (mapcar #'first dict) RETURNS (ONE TWO THREE FOUR FIVE)
;;; (mapcar #'second dict) RETURNS (UN DEUX TROIS QUATRE CINQ)
;;; (mapcar #'reverse dict) RETURNS ((UN ONE) (DEUX TWO) (TROIS THREE) (QUATRE FOUR) (CINQ FIVE))

;;; exercise 7.1
;;; function adds one to its input, call with mapcar to apply to each symbol in list
;;; (mapcar #'add1 '(1 3 5 7 9) RETURNS (2 4 6 8 10)
(defun add1 (number)
	(+ number 1))

;;; exercise 7.2
(setf daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
					(kent clark 089-52-6787 reporter)
					(lane lois 951-26-1438 reporter)
					(white perry 355-16-7439 editor)))

;;; to get social securities from table:
;;; (mapcar #'third daily-planet) RETURNS (123-76-4535 089-52-6787 951-26-1438 355-16-7439)

;;; exercise 7.7
;;; converts up to down and down to up, any other input in a list returns as nil
(defun reverse-up-down (lst)
	(mapcar #'(lambda (x)
		(if (equal x 'up) 
		'down (if (equal x 'down) 'up nil))) lst))

;;; this is a custom function that works just like how assoc does
(defun my-assoc (key table)
	(find-if #'(lambda (entry)
					(equal key (first entry))) table))

;;; exercise 7.8
;;; returns the first element in list x that is no more than k+10 and no less than k-10
(defun rough-equal (x k)
	(find-if #'(lambda (z)
		(if (and (> (+ k 10) z) 
			(< (- k 10) z)) z nil)) x))

;;; exercise 7.9
;;; returns the first element of a list that is not itself a non-NIL list.
(defun find-nested (lst)
	(find-if #'(lambda (cell) 
		(if (and (listp cell) 
			(not (null cell))) cell nil)) lst))

;;; exercise 7.11
;;; returns all numbers in a list that are greater than 1 and less than 5.
(defun btw-one-and-five (lst)
	(remove-if-not #'(lambda (atm)
		(if (and 
			(numberp atm) 
			(< atm 5)
			(> atm 1)) atm nil)) lst))

;;; exercise 7.12
;;; returns the number of times the word "the" is contained in a list.
(defun count-of-the (lst)
	(length (remove-if-not #'(lambda (word)
		(if (equal word 'the) word nil)) lst)))

;;; exercise 7.13
;;; returns a list containing only sublists of length two.
(defun remove-nonlentwo-lists (lst)
	(remove-if-not #'(lambda (sublst)
		(if (eql (length sublst) 2) sublst nil)) lst))

;;; exercise 7.14
;;; returns the set difference
(defun my-setdiff (x y)
	(remove-if #'(lambda (e) (member e y)) x))

;;; returns the intersection
(defun my-intersection (x y)
	(remove-if-not #'(lambda (e) (member e y)) x))

;;; returns the union
(defun my-union (x y)
	(append x (remove-if #'(lambda (e) (member e x)) y)))


;;; exercise 7.17
;;; given a list of lists, returns the total length of all lists.
(defun nested-list-length (nested_list)
	(reduce #'+ (mapcar #'length nested_list)))

;;; exercise 7.19
;;; returns T if every element of list of numbers are odd, NIL otherwise.
(defun all-odd (numbers)
	(and (every #'numberp numbers)
	(every #'oddp numbers)))

;;; exercise 7.20
;;; returns T if every element of a list of numbers is not odd, NIL otherwise.
(defun none-odd (numbers)
	(and (every #'numberp numbers)
	(every #'(lambda (x) (= (mod x 2) 0)) numbers)))

;;; exercise 7.21 
;;; returns T if not every element of a list of numbers is odd, NIL otherwise.
(defun not-all-odd (numbers)
	(and (every #'numberp numbers)
	(not (every #'oddp numbers))))

;;; exercise 7.22
;;; returns T if it is not the case that a list of numbers contains no odd elements, NIL otherwise.
(defun not-none-odd (numbers)
	(and (every #'numberp numbers)
	(not (every #'evenp numbers))))


















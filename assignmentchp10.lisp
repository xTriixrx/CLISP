;;; Global variable total-glasses for lemonade stand problem.
(setf *total-glasses* 0)

;;; Global variable friends for meet problem.
(setf *friends* nil)

;;; Global variable Exercise 10.3
(setf *met-more-than-once* 0)

;;; Exercise 10.2
;;; This function is a rewrite to the lemonade stand SELL function using INCF instead of SETF.
(defun sell (n)
	"Ye Olde Lemonade Stand: Sales by the Glass."
	(incf *total-glasses* n)
	(format t "~&That makes ~S glasses so far today." *total-glasses*))

;;; Exercise 10.3
;;; This function modifys the MEET function to keep a count of how many people have been met more than once.
(defun meet (person)
	(cond ((equal person (first *friends*))
			(incf *met-more-than-once* 1) 'we-just-met)
		((member person *friends*) (incf *met-more-than-once* 1) 'we-know-each-other)
		(t (push person *friends*) 'pleased-to-meet-you)))

(defun met-more-than-once ()
	(format t "There has been ~S times people have been met more than once." *met-more-than-once*))

;;; Exercise 10.4
;;; This function removes a person from the *FRIENDS* list. (Handled if the person was not on the list
;;; because I didn't want the function to 'complain'.
(defun forget (person)
	(if (member person *friends*)
	(setf *friends* (remove person *friends*))
	'No-Match))

;;; Exercise 10.9
;;; This destructive function shortens any non-NIL list to a list of one element.
(defun chop (lst)
	(setf lst (list (car lst))))

;;; Exercise 10.10
;;; This destructive function adds a symbol onto a list.
(defun ntack (lst atm)
	(nconc lst (list atm)))
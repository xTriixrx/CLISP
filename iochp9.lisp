;;; Exercise 9.1
;;; This function tests the format function and printing on separate lines.
(defun type-of-pilots ()
	(format t "~&There are old pilots," )
	(format t "~&and there are bold pilots," )
	(format t "~&but there are no old bold pilots." ))

;;; Exercise 9.2
;;; This function is a recursive function that draws a line of n size.
(defun draw-line (n)
	(cond ((> 1 n) nil)
		(t (format t "*" (draw-line (- n 1))))))

;;; Exercise 9.2
;;; This function recursively draws a box using the draw line function.
(defun draw-box (x y)
	(cond ((> 1 y) nil)
		(t (format t "~&" (draw-box x (- y 1)) (draw-line x)))))

;;; Exercise 9.6
;;; This function asks for user input to compute the hourly worker's gross pay.
(defun gross-pay ()
	(format t "Please enter your hourly rate and hours worked:")
	(let* ((rate (read))
		(hours (read))
		(gross-pay (* 1.0 (* rate hours))))
	(format t "You earned $~S in ~S hours this week." gross-pay hours)))

;;; This version of gross-pay separates the formatting of asking input from user.

;(defun gross-pay ()
;	(format t "Please enter your hourly rate: ")
;	(let ((rate (read)))
;	(format t "Please enter the hours you worked: ")
;	(let ((hours (read)))
;	(format t "You earned $~S in ~S hours this week." (* rate hours) hours))))

;;; Reading from file example (using timber.dat within same directory as this file)
;;; If reading from another location such as /usr/dst/.. provide path.
(defun get-tree-data ()
	(with-open-file (stream "timber.dat")
		(let* ((tree-loc (read stream))
			(tree-table (read stream))
			(num-trees (read stream)))
		(format t "~&There are ~S trees on ~S." num-trees tree-loc)
		(format t "~&They are:	~S" tree-table))))

;;; Writing to file example (creates timber.newdat within same directory as this file)
;;; If want another location specify the path.
;;; Function was called with : (save-tree-data "The West Ridge" '((45 redwood) (22 oak) (43 maple)) 110)
(defun save-tree-data (tree-loc tree-table num-trees)
	(with-open-file (stream "timber.newdat" :direction :output)
		(format stream "~S~%" tree-loc)
		(format stream "~S~%" tree-table)
		(format stream "~S~%" num-trees)))

;;; This function shows how to use the formatting of a decimal place
(defun sevenths (x)
	(mapcar #'(lambda (numerator)
				(format t "~&~4,2F / 7 is ~7,5F" numerator (/ numerator 7.0)))
			x) 'done)


;;; These functions below are used in order to read an aribitarily long file of data and not generate
;;; an EOF error. This is done by providing a special EOF value.
;;; Read-my-file is called such as: (read-my-file "providepath.dat")
;;; The helper function read-all-objects uses the eof-indicator to signal and silence the EOF exception,
;;; and trigger when the file is done being read. Uses cons list recursion to build up the items in the
;;; file. Also specifies how many objects were read from a file.	
(defun read-my-file (file)
	(labels ((read-all-objects (stream eof-indicator)
		(let ((result (read stream nil eof-indicator)))
		(if (eq result eof-indicator) nil (cons result (read-all-objects stream eof-indicator))))))
	(with-open-file (stream file)
		(let ((contents (read-all-objects stream (list '$eof$))))
		(format t "~&Read ~S objects from the file." (length contents)) contents))))


;;; Exercise 9.11
;;; This function takes a list as input and prints it in dot notation.
;;; EX: (A) -> (A . NIL) (A (B) C) -> (A . ((B . NIL) . (C . NIL)))
(defun dot-prin1 (lst)
	(cond ((null lst) (format t "~S" nil))
		((atom lst) (format t "~S" lst))
		((and (consp lst) (atom (car lst))) 
			(format t "(~S  .  " (car lst)) 
				(dot-prin1 (cdr lst)) 
				(format t ")"))
		(t (format t "(") 
			(dot-prin1 (car lst)) 
			(format t "  .  ") 
			(dot-prin1 (cdr lst)) 
			(format t ")"))) nil)

;;; Exercise 9.15
;;; This function takes an input list and prints its hybrid notation.
;;; EX: (A . NIL) -> (A) (A . B) -> (A . B) ((A . NIL) . (B . (C . D))) -> ((A) B C . D)
;;; EVEN MORE TRICKY THAN THE PREVIOUS
(defun hybrid-prin1 (lst)
	(labels ((print-cdr (sub-lst)
		(cond ((null (cdr sub-lst)) (format t ")"))
			((atom lst) (format t "  .  ~S)" lst))
			(t (format t "  .  ~S)" (print-cdr (cdr sub-lst)))))))
		(cond ((null lst) nil)
			((atom lst) (format t "(~S" lst))
			((listp lst) (hybrid-prin1 (car lst)) (print-cdr (hybrid-prin1 (cdr lst))))
			(t (hybrid-prin1 (car lst)) (print-cdr (cdr lst)))))nil)















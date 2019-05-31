;;; How to add append element to end of a list
;;; (add-to-end '(A B C) 'D) RETURNS (A B C D)
;;;		(append '(A B C) 'D) RETURNS (A B C . D)
(defun add-to-end (lst element)
	(append lst (list element)))

;;; exercise 6.6
;;; using last func return last element of a list
(defun last-last-element (lst)
	(car (last lst)))

;;; using reverse func return last element of a list
(defun reverse-last-element (lst)
	(car (reverse lst)))

;;; using nth and length func to return last element of a list
(defun nlength-last-element (lst)
	(nth (- (length lst) 1) lst))


;;; exercise 6.7
;;; use reverse func to return next to last element of a list
(defun reverse-nxttolast-element (lst)
	(cadr (reverse lst)))

;;; use nth func to return next to last element of a list
(defun nlength-nxttolast-element (lst)
	(nth (- (length lst) 2) lst))


;;; exercise 6.8
;;; returns a list with the last element removed
(defun my-butlast (lst)
	(reverse (cdr (reverse lst))))


;;; exercise 6.10
;;; return t if list is a palindrome and nil otherwise
(defun palindrome-p (lst)
	(let ((rvs (reverse lst)))
		(if (equal rvs lst) t nil)))


;;; exercise 6.11
;;; returns a palindrome by making one out of a given list
(defun mke-palindrome (lst)
	(let ((rvs (reverse lst)))
		(append lst rvs)))


;;; exercise 6.18
;;; returns the union of a passed set and vowels
(defun add-vowels (lst)
	(let ((vowels (list 'a 'e 'i 'o 'u)))
		(union lst vowels)))

;;; exercise 6.21 
;;; returns t if first input is a subset of second input
(defun my-subsetp (lst1 lst2)
	(let ((difference (set-difference lst1 lst2)))
		(if (null difference) t nil)))

;;; exercise 6.24 
;;; returns t if sets are equal to each other and nil otherwise
(defun set-equal (set1 set2)
	(let ((diff1 (set-difference set1 set2))
			(diff2 (set-difference set2 set1)))
		(if (and (null diff1) (null diff2)) t nil)))

;;; exercise 6.25
;;; returns t if set1 is a proper subset of set2 and nil otherwise
(defun proper-subsetp (set1 set2)
	(let ((diff1 (set-difference set1 set2))
			(diff2 (set-difference set2 set1)))
		(if (and (null diff1) (not (null diff2))) t nil)))


;;; exercise 6.31
;;; REQUIREMENT: need BOOKS variable created such as:
;;; ((close-to-the-machine ellen-ullman) (the-art-of-unix-programming eric-raymond)
;;;  (the-art-of-computer-programming donald-knuth) (free-software,-free-society richard-stallman)
;;;  (after-the-software-wars keith-curtis))

;;; books is global variable 
(setf books '((close-to-the-machine ellen-ullman) (the-art-of-unix-programming eric-raymond)
	(the-art-of-computer-programming donald-knuth) (free-software-free-society richard-stallman)
	(after-the-software-wars keith-curtis)))

;;; returns the author of a book from the title of a book
(defun who-wrote (title)
	(cdr (assoc title books)))

;;; exercise 6.36
;;; returns a list of the first and last elements swapped from the input list.
(defun swap-first-last (lst)
	(let* ((fst (car lst))
			(end (car (last lst)))
			(remaining (remove fst lst :count 1))
			(remaining (remove end remaining :count 1))
			(remaining (append remaining (list fst))))
		(reverse (append (reverse remaining) (list end)))))


;;; exercise 6.37
;;; returns a left shifted list from the input list.
(defun rotate-left (lst)
	(let* ((fst (car lst))
			(remaining (remove fst lst :count 1)))
		(append remaining (list fst))))

;;; returns a right shifted list from the input list.
(defun rotate-right (lst)
	(let* ((end (car (last lst)))
			(remaining (reverse (remove end (reverse lst) :count 1))))
		(cons end remaining)))


;;; exercise 6.42
;;; changes every occurrence of I to WE.
(defun royal-we (sentence)
	(subst 'we 'i sentence))





















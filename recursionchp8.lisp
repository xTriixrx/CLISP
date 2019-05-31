;;; RECURSION CHAPTER

;;; This function returns T if any element of a list of numbers is odd, NIL otherwise.
(defun anyoddp (x)
	(cond ((null x) nil)
		((oddp (first x)) t)
		(t (anyoddp (rest x)))))

;;; Exercise 8.2
;;; Same functionality as anyoddp but done with an if statement.
(defun if-anyoddp (x)
	(if (null x) nil (if (oddp (first x)) t (if-anyoddp (rest x)))))

;;; Exercise 8.4
;;; This function returns the number of laughs as given input x (LAUGH 0) is nil, (LAUGH 2) is (HA HA).
(defun recursive-laugh (n)
	(cond ((zerop n) nil)
		(t (cons 'HA (recursive-laugh (- n 1))))))

;;; Exercise 8.5
;;; This function adds up all the numbers in a list.
(defun add-all (x)
	(cond ((null x) 0)
		(t (+ (car x) (add-all (rest x))))))

;;; Exercise 8.6
;;; a recursive predicate function that returns true if all numbers in list are odd, NIL otherwise.
(defun alloddp (x)
	(cond ((null x) t)
		((evenp (first x)) nil)
		(t (alloddp (rest x)))))

;;; Exercise 8.11
;;; This function returns the fibonacci sequence.
(defun my-fibonacci (n)
	(cond ((equal n 0) 1)
		((equal n 1) 1)
		(t (+ (my-fibonacci (- n 1)) (my-fibonacci (- n 2))))))

;;; Exercise 8.17
;;; This function returns the first odd number in a list, NIL otherwise.
(defun find-first-odd (x)
	(cond ((null x) nil)
		((oddp (car x)) (car x))
		(t (find-first-odd (cdr x)))))

;;; Exercise 8.18
;;; This function recursively returns the last element in a list.
(defun last-element (x)
	(cond ((equal (length x) 1) (car x))
		(t (last-element (rest x)))))

;;; Exercise 8.21
;;; This function recursively adds up the numbers N, N-1, N-2, ... and so on down to 0.
(defun add-nums (x)
	(cond ((equal x 0) 0)
		(t (+ x (add-nums (- x 1))))))

;;; Exercise 8.22
;;; This function recursively checks if all atoms in a list are equal to each other, T if true NIL otherwise.
(defun all-equal (x)
	(cond ((equal (length x) 1) t)
		((not (equal (first x) (second x))) nil)
		(t (all-equal (rest x)))))

;;; Exercise 8.24
;;; This function recursively counts down from n building a list using list consing recursion.
(defun count-down (n)
	(cond ((zerop n) nil)
	(t (cons n (count-down (- n 1))))))

;;; Exercise 8.25
;;; This applicative factorial function uses the count-down function from the previous exercise.
(defun applicative-fact (n)
	(reduce #'* (count-down n)))

;;; Exercise 8.27
;;; This function takes a list of numbers and returns a list of their squares.
(defun square-list (x)
	(cond ((null x) nil)
		(t (cons (* (car x) (car x)) (square-list (rest x))))))

;;; Exercise 8.28
;;; This function works like nth, and has been modified to exit recursion as quickly as possible.
(defun my-nth (n x)
	(cond ((zerop n) (first x))
		((null x) nil)
		(t (my-nth (- n 1) (rest x)))))

;;; Exercise 8.29
;;; This function is a recursive equivalent to the standard member function.
(defun my-member (item lst)
	(cond ((null lst) nil)
		((equal item (car lst)) lst)
		(t (my-member item (cdr lst)))))

;;; Exercise 8.30
;;; This function is a recursive equivalent to the standard member function.
(defun my-assoc (key table)
  (cond ((null table) nil)
	((equal key (caar table)) (car table))
	(t (my-assoc key (cdr table)))))

;;; Exercise 8.31
;;; This function recursively compares the lengths of 2 lists and doesn't use the length function.
(defun compare-lengths (lst1 lst2)
	(cond ((and (null lst1) (not (null lst2))) (list 'SECOND-IS-LONGER))
		((and (not (null lst1)) (null lst2)) (list 'FIRST-IS-LONGER))
		((and (null lst1) (null lst2)) (list 'SAME-LENGTH))
		(t (compare-lengths (rest lst1) (rest lst2)))))

;;; Exercise 8.32
;;; This function recursively adds up all the numbers in a list and ignores the non-numbers.
(defun sum-numeric-elements (lst)
	(cond ((null lst) 0)
		((numberp (car lst)) (+ (car lst) (sum-numeric-elements (cdr lst))))
		(t (sum-numeric-elements (cdr lst)))))

;;; Exercise 8.33
;;; This function is the recursive equivalent to the standard remove function.
(defun my-remove (item lst)
	(cond ((null lst) nil)
		((equal (car lst) item) (my-remove item (cdr lst)))
		(t (cons (car lst) (my-remove item (cdr lst))))))

;;; Exercise 8.39
;;; This function recursively counts the number of atoms in a tree (includes nil).
(defun count-atoms (tree)
	(cond ((or (null tree) (atom tree)) 1)
		(t (+ (count-atoms (car tree)) (count-atoms (cdr tree))))))

;;; Exercise 8.40
;;; This function recursively counts the number of cons cells in a structure.
(defun count-cons (tree)
	(cond ((atom tree) 0)
		(t (+ 1 (count-cons (car tree)) (count-cons (cdr tree))))))

;;; Exercise 8.41
;;; This function recursively adds the numbers appearing in a tree structure.
(defun sum-tree (tree)
	(cond ((numberp tree) tree)
		((atom tree) 0)
		(t (+ (sum-tree (car tree)) (sum-tree (cdr tree))))))

;;; Exercise 8.42
;;; This function is the recursive equivalent to the standard subst function.
(defun my-subst (new old lst)
  (cond ((equal lst old) new)
	((atom lst) lst)
	(t (cons (my-subst new old (car lst))
		 (my-subst new old (cdr lst))))))

;;; Exercise 8.43
;;; This function recursively takes an arbitarily nested list and converts it to a single-level list.
(defun flatten (nested-list)
  (cond ((atom nested-list) (list nested-list)) 
       	(t (append (flatten (car nested-list))
		   (and (cdr nested-list) (flatten (cdr nested-list)))))))

;;; Exercise 8.48
;;; This function recursively adds parentheses based on input passed from user.
(defun bury (item n)
	(labels ((bury-recursively (cnt n item)
	(cond ((> cnt n) item)
		(t (list (bury-recursively (+ cnt 1) n item))))))
	(bury-recursively 1 n item)))

;;; Exercise 8.54
;;; This function raises a number to its own power, 3^3 = 27 and so on.			
(defun raise-itself (n)
	(labels ((raise-itself-recursively (cnt n)
	(cond ((> cnt n) 1)
		(t (* n (raise-itself-recursively (+ cnt 1) n))))))
	(raise-itself-recursively 1 n)))

;;; Exercise 8.61
;;; This function is a tail recursive version of count-up
(defun tail-rec-count-up (total)
	(labels ((tr-count-up (total count)
	(cond ((null total) count)
		(t (tr-count-up (cdr total) (+ 1 count))))))
	(tr-count-up total 0)))

;;; Exercise 8.62
;;; This function is a tail recursive version of factorial
(defun my-fact (n)
	(labels ((tr-fact (n product)
	(cond ((zerop n) product)
		(t (tr-fact (- n 1) (* product n))))))
	(tr-fact n 1)))

;;;Exercise 8.64
;;; This function returns the first non-NIL atom of a tree that satisfies a predicate.
(defun tree-find-if (fn x)
	(cond ((null x) x)
		((atom x) (if (funcall fn x) x nil))
		(t (or (tree-find-if fn (car x)) (tree-find-if fn (cdr x))))))

;;; Exercise 8.66
;;; This function evaluates arithmetic expressions such as: (arith-eval '(2 + (3 * (4 + 5))))
(defun arith-eval (expr)
  (cond ((null expr) nil)
	((numberp expr) expr)
	((equal 3 (length expr))
	 (funcall (cadr expr)
		  (arith-eval (car expr))
		  (arith-eval (caddr expr))))
	(t nil)))

;;; Exercise 8.66
;;; This function returns T if input is a legal arithmetic expression and nil otherwise.
(defun legalp (expr)
	(not (null (arith-eval expr))))

;;; Exercise 8.70
;;; This function returns a factorization tree
(defun factor-tree (n)
	(labels ((factor-tree-help (n p)
		(cond ((equal n p) n)
			((zerop (rem n p))
			  (list n p (factor-tree-help (/ n p) p)))
			(t (factor-tree-help n (+ p 1))))))
	(factor-tree-help n 2)))
			

;;; This function returns the factorial of the passed parameter.
(defun my-factorial (n)
	(cond ((zerop n) 1)
		(t (* n (my-factorial (- n 1))))))


;;; Exercise 1.1
;;; Medium
;;; Define a version of 1 ast-name that handles "Rex Morgan MD,"
;;; "Morton Downey, Jr.," and whatever other cases you can think of.
(defparameter *end-titles* '(MD Jr. Jr JR SR Sr. Sr The I II III IV V VI)
 "A list of titles that can appear at the start of a name.")

(defun last-name (name)
 "Select the last name from a name represented as a list."
 (if (member (first (last name)) *end-titles*)
     (last-name (reverse (cdr (reverse name)))) (first (last name))))

;;; Exercise 1.2
;;; Medium
;;; Write a function to exponentiate, or raise a number to an integer
;;; power. For example: (power 3 2) = 3^ = 9.
(defun power (x n)
 "Power raises x to the nth power. Ν must be an integer >= 0.
This executes in log n time, because of the check for even n."
 (cond ((= n 0) 1)
   ((evenp n) (expt (power x (/ n 2)) 2))
   (t (* x (power x (- n 1))))))

;;; Exercise 1.3
;;; Medium
;;; Write a function that counts the number of atoms in an expression. For
;;; example: (count-atoms '(a (b) c)) = 3. Notice that there is something
;;; of an ambiguity in this: should (a nil c) count as three atoms, or as
;;; two, because it is equivalent to (a () c)?
(defun count-atoms (x)
 "Recursively counts the atoms of a list."
  (cond ((null x) 0)
    ((atom x) 1)
    (t (+ (count-atoms (first x))
          (count-atoms (rest x))))))

;;; Exercise 1.4
;;; Medium
;;; Write a function that counts the number of times an expression occurs
;;; anywhere within another expression. Example:
;;; (count-anywhere 'a '(a ((a) b) a)) => 3.
(defun count-anywhere (exp full-exp)
 "Counts number of times exp appears in the full expression."
  (cond ((equal exp full-exp) 1)
        ((atom full-exp) 0)
   (t (+ (count-anywhere exp (first full-exp))
         (count-anywhere exp (rest full-exp))))))

;;; Exercise 1.5
;;; Medium
;;; Write a function to compute the dot product of two sequences of numbers,
;;; represented as lists. The dot product is computed by multiplying
;;; corresponding elements and then adding up the resulting products.
;;; Example: (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110
(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))

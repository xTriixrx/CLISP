;;; Exercise 11.1
;;; This function is an iterative version of the member function, returns T if its first input
;;; appears in the second input, NIL otherwise.
(defun it-member (find search)
	(dolist (x search nil)
		(when (equal find x)
			(return t))))

;;; Exercise 11.2
;;; This function is an iterative version of the assoc function.
(defun it-assoc (symb table)
	(dotimes (i (length table) nil)
		(when (equal symb (car (nth i table)))
			(return (nth i table)))))

;;; Exercise 11.3
;;; This function is a recursive version of check-all-odd.
(defun check-all-odd (lst-nums)
	(if (not (null lst-nums)) (format t "~&Checking ~S..." (first lst-nums)))
	(cond ((null lst-nums) t)
		((not (oddp (first lst-nums))) nil)
		(t (check-all-odd (cdr lst-nums)))))

;;; Exercise 11.4
;;; This function is a iterative version of length.
(defun it-length (lst)
	(let ((count 0))
		(dolist (atm lst count)
			(incf count 1))))

;;; Exercise 11.5
;;; This function is an iterative version of nth.
(defun it-nth (pos lst)
	(let ((count 0))
		(dolist (atm lst 'nil)
			(when (equal pos count)
				(return atm))
			(incf count 1))))

;;; Exercise 11.6
;;; This function is an iterative version of union.
(defun it-union (lst1 lst2)
	(let ((len1 (length lst1))
		(len2 (length lst2))
		(result-set nil))
	(dotimes (i (+ len1 len2))
		(if (and (not (null (nth i lst1))) 
				(not (member (nth i lst1) result-set))) 
			(push (nth i lst1) result-set))
		(if (and (not (null (nth i lst2))) 
				(not (member (nth i lst2) result-set))) 
			(push (nth i lst2) result-set))
		(if (and (null (nth i lst1)) 
			(null (nth i lst2))) 
		(return result-set)))))

;;; Exercise 11.8
;;; This function is an iterative version of reverse.
(defun it-reverse (lst)
	(let ((rv-list nil))
		(dolist (atm lst rv-list)
			(push atm rv-list))))

;;; Exercise 11.9
;;; This function is how to do check-all-odd with a do.
(defun do-check-all-odd (lst)
	(do ((nums lst (rest nums)))
		((null (car nums)) t)
		(format t "~&Checking ~S..." (car nums))
 		(if (evenp (car nums)) (return nil))))
		

;;; Exercise 11.10
;;; This function is the LAUNCH function using DOTIMES.
(defun launch (n)
	(dotimes (i 11)
		(when (zerop (- 10 i))
			(format t "Blast off!")
			(return nil))
		(format t "~S..." (- 10 i))))

;;; Exercise 11.11
;;; This function is a rewrite of the find-largest function that uses a
;;; dolist instead of a do*.
(defun find-largest (list-nums)
	(do* ((nums list-nums (rest nums))
		(element (first nums) (first nums))
		(largest -9999999))
		((null nums) largest)
		(when (> element largest)
			(setf largest element))))

;;; Exercise 11.12
;;; This function is a rewrite of the power-of-2 function that uses a
;;; dotimes instead of a do. (2 to the nth power)
(defun power-of-2 (n)
	(do ((cnt 1 (+ cnt 1))
		(result 1 (incf result result)))
		((equal cnt n) result)))

;;; Exercise 11.13
;;; This is a rewrite of the first-non-integer function that uses a
;;; do* instead of a dolist.
(defun first-non-integer (x)
	(dolist (element x 'none)
		(unless (numberp element)
			(return element))))

;;; Exercise 11.21
;;; This function is an iterative version of the fibonacci sequence.
(defun it-fibonacci (n)
	(let ((fiblist (list 0 1 1)))
	(dotimes (i n (first fiblist))
		(nconc fiblist (list (+ (second fiblist) (third fiblist))))
		(pop fiblist))))


;;; KEYBOARD EXERCISE 11.22 Properties of single and double stranded DNA.

;;; Exercise 11.22 Part A 
;;; This function COMPLEMENT-BASE takes a base as input and returns the matching complementary base.
;;; (COMPLEMENT-BASE 'A) should return T.
(defun complement-base (base)
	(let ((pairs (list 'A 'T 'G 'C))
		(count 0))
		(dolist (b pairs 'nil)
			(if (equal b base) 
				(if (equal (mod count 2) 0)
					(return (nth (+ count 1) pairs))
					(return (nth (- count 1) pairs))))
			(incf count 1))))

;;; Exercise 11.22 Part B
;;; This function COMPLEMENT-STRAND that returns the complementary
;;; strand of a sequence of a single stranded DNA.
;;; (COMPLEMENT-STRAND '(A G G T)) should return (T C C A).
(defun complement-strand (single-dna)
	(let ((complement-list (list nil)))
		(dotimes (i (length single-dna) (rest complement-list))
			(nconc complement-list (list (complement-base (nth i single-dna)))))))

;;; Exercise 11.22 Part C
;;; This function MAKE-DOUBLE takes a single strand of DNA as input
;;; and returns a double-stranded version.
;;; (MAKE-DOUBLE '(G G A C T)) should return ((G C) (G C) (A T) (C G) (T A)).
(defun make-double (single-dna)
	(do ((complement-list (complement-strand single-dna))
		(double-strand (list nil))
		(size (length single-dna))
		(count 0 (+ count 1)))
		((equal count size) (rest double-strand))
		(nconc double-strand (list (list (nth count single-dna) (nth count complement-list))))))

;;; Exercise 11.22 Part D
;;; This function COUNT-BASES counts the number of bases of each type
;;; in a single or double strand of dna (Added in flatten helper function to make problem easier).
;;; (COUNT-BASES '((G C) (A T) (T A) (T A) (C G))) returns ((A 3) (T 3) (G 2) (C 2)).
;;; (COUNT-BASES '(A G T A C T C T)) returns ((A 2) (T 3) (G 1) (C 2)).

(defun count-bases (dna)
	(labels ((flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node)))))))
	(do* ((adenine 0)
		(thymine 0)
		(guanine 0)
		(cytosine 0)
		(flatten-dna (flatten dna))
		(size (length flatten-dna))
		(count 0 (incf count 1))
		(result nil))
		((equal count size) (list (list 'A adenine) (list 'T thymine)
		(list 'G guanine) (list 'C cytosine)))
		(if (equal (nth count flatten-dna) 'A) (incf adenine 1)
		(if (equal (nth count flatten-dna) 'T) (incf thymine 1)
		(if (equal (nth count flatten-dna) 'G) (incf guanine 1)
		(if (equal (nth count flatten-dna) 'C) (incf cytosine 1))))))))

;;; Exercise 11.22 Part E
;;; This function PREFIXP returns T if one strand of DNA is a prefix of another. Prefix meaning that
;;; the elements of the first strand must exactly match the corresponding elements of the second which
;;; can be longer than the first strand. EX: (G T C) is a prefix of (G T C A T) but not (A G G T C).
(defun prefixp (strand1 strand2)
	(let ((count 0))
		(dolist (e strand1 t)
			(unless (equal e (nth count strand2))
				(return nil))
			(incf count 1))))

;;; Exercise 11.22 Part F
;;; This function APPEARSP returns T if on DNA strand appears anywhere within another. For example,
;;; (C A T) appears in (T C A T G) but not in (T C C G T A).
(defun appearsp (strand1 strand2)
	(do ((strand strand2 (rest strand)))
		((null strand) nil)
		(when (prefixp strand1 strand)
			(return t))))

;;; Exercise 11.22 Part G
;;; This function COVERP returns T if the first input, repeated some number of times, matches all of its
;;; second input. EX: (A G C) covers (A G C A G C A G C) but not (A G C T T G).
(defun coverp (pattern strand)
	(let ((count 0)
		(pat-len (length pattern)))
	(dotimes (i (length strand) t)
		(unless (equal (nth count pattern) (nth i strand))
			(return nil))
		(incf count 1)
		(when (equal count pat-len)
			(setf count 0)))))

;;; Exercise 11.22 Part H
;;; This function PREFIX returns the leftmost N bases of a DNA strand. For example, (PREFIX 4 '(C G A T T A G)
;;; should return a list of (C G A T).
(defun prefix (num dna)
	(do ((prefix-list (list nil))
		(count 0 (incf count 1)))
		((equal count num) (rest prefix-list))
		(nconc prefix-list (list (nth count dna)))))

;;; Exercise 11.22 Part I
;;; This function KERNEL returns the shortest prefix of a DNA strand tha can be repeated to cover the strand.
;;; For example, (KERNEL '(A G C A G C A G C)) should return (A G C), (KERNEL '(A A A A)) should return (A), and
;;; (KERNEL '(A G G T C)) should return (A G G T C).
;;; HINT LOOK AT PREFIX OF INCREASING LENGTH UNTIL FOUND ON THAT CAN BE REPEATED.
(defun kernel (dna)
	(do* ((count 1 (incf count 1))
		(pref (prefix 1 dna) (prefix count dna)))
		((coverp pref dna) pref)))

;;; Exercise 11.22 Part J
;;; This function DRAW-DNA takes a signle-stranded DNA sequence as input and draws it along with its 
;;; complementary strand.
(defun draw-dna (single-strand)
	(let ((count 0)
		(maxiter (* 8 (length single-strand)))
		(comp (complement-strand single-strand)))
	(dotimes (i (* 8 (length single-strand)))
		(cond ((< i (/ maxiter 8)) (format t "-----"))
			((= i (/ maxiter 8)) (format t "~%  !  "))
			((< i (* 2 (/ maxiter 8))) (format t "  !  "))
			((= i (* 2 (/ maxiter 8))) (format t "~%  ~S  " (first single-strand)) (incf count 1))
			((< i (* 3 (/ maxiter 8))) (format t "  ~S  " (nth count single-strand)) (incf count 1))
			((= i (* 3 (/ maxiter 8))) (format t "~%  .  ") (setf count 0))
			((< i (* 4 (/ maxiter 8))) (format t "  .  "))
			((= i (* 4 (/ maxiter 8))) (format t "~%  .  "))
			((< i (* 5 (/ maxiter 8))) (format t "  .  "))
			((= i (* 5 (/ maxiter 8))) (format t "~%  ~S  " (first comp)) (incf count 1))
			((< i (* 6 (/ maxiter 8))) (format t "  ~S  " (nth count comp)) (incf count 1))
			((= i (* 6 (/ maxiter 8))) (format t "~%  !  "))
			((< i (* 7 (/ maxiter 8))) (format t "  !  "))
			((= i (* 7 (/ maxiter 8))) (format t "~%-----"))
			(t (format t "-----"))))))


;; exercise 4.1 
(defun make-even (num)
	(if (oddp num) (+ num 1) num))

;; exercise 4.2
(defun further (num)
	(if (> num 0) (+ num 1) (- num 1)))

;; exercise 4.3
(defun my-not (input)
	(if (equal input T) NIL T))

;; exercise 4.4
(defun ordered (x y)
	(if (< x y) (list x y) (list y x)))

;; exercise 4.6
(defun my-abs (x)
	(cond ((< x 0) (* x -1))
		(T x)))

;; exercise 4.8
(defun emphasize3 (x)
	(cond ((equal (first x) 'good) (cons 'great (rest x)))
		((equal (first x) 'bad) (cons 'awful (rest x)))
		(T (cons 'very x))))

;; part a of exercise 4.10
(defun constrain (x minimum maximum)
	(cond ((< x minimum) minimum)
		((> x maximum) maximum)
		(T x)))

;; part b of exercise 4.10
;;(defun constrain (x minimum maximum)
;;	(if (< x minimum) minimum (if (> x maximum) maximum x

;; exercise 4.11
(defun firstzero (x)
	(cond ((= (first x) 0) 'first)
		((= (second x) 0) 'second)
		((= (third x) 0) 'third)
		(T 'none)))

;; exercise 4.12
(defun cycle (num)
	(cond ((> num 99) NIL)
		((= num 99) 1)
		(T (+ num 1))))
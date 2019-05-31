;;; exercise 5.1
(defun good-style (p)
	(let ((q (+ p 5)))
		(list 'result 'is q)))

;;; This shows the correct semicolon documentation in CLISP: OUTSIDE OF FUNCTION & BEGIN LEFT MARGIN
;;; coin toss simulation
(defun coin-toss ()
	"COIN-TOSS: Simulates a coin toss by using RANDOM between 1 and 100."
	(let ((toss (random 101))) ; COMMENT TO THE RIGHT OF A LINE
		;; COMMENTS THAT OCCUPY THEIR OWN LINE
		(cond ((< toss 50) 'heads)
			((> toss 50) 'tails)
			(t 'edge))))

;;; exercise 5.6 part a
(defun throw-die ()
	(let ((toss (+ 1 (random 6))))
		(cons toss nil)))

;;; exercise 5.6 part b
(defun throw-dice ()
	(let ((die1 (throw-die))
		(die2 (throw-die)))
		(list (car die1) (car die2))))

;;; exercise 5.6 part c
(defun snake-eyes-p (roll)
	(if 
		(and 
			(equal (first roll) 1) 
			(equal (second roll) 1)) 
		t nil))

(defun boxcars-p (roll)
	(if 
		(and 
			(equal (first roll) 6) 
			(equal (second roll) 6)) 
		t nil))

;;; exercise 5.6 part d
(defun instant-win-p (roll)
	(if 
		(or 
			(equal (+ (first roll) (second roll)) 7) 
			(equal (+ (first roll) (second roll)) 11)) 
		t nil))

(defun instant-loss-p (roll)
	(if 
		(or 
			(equal (+ (first roll) (second roll)) 2) 
			(equal (+ (first roll) (second roll)) 3) 
			(equal (+ (first roll) (second roll)) 12)) 
		t nil))

;;; exercise 5.6 part e
(defun say-throw (roll)
	(cond ((boxcars-p roll) 'BOXCARS)
		((snake-eyes-p roll) 'SNAKE-EYES)
		(t (+ (first roll) (second roll)))))

;;; exercise 5.6 part f
(defun craps ()
	(let* ((roll (throw-dice))
		(die1 (first roll))
		(die2 (second roll))
		(craps (list 'throw die1 'and die2 '--)))
	(cond ((equal (instant-loss-p roll) t)
			(if (boxcars-p roll) (append craps (list (say-throw roll) '-- 'YOU 'LOSE))
				(if (snake-eyes-p roll) 
					(append craps (list (say-throw roll) '-- 'YOU 'LOSE)) 
					(append craps (list (say-throw roll) '-- 'YOU 'LOSE)))))
		((equal (instant-win-p roll) t)
			(append craps (list (say-throw roll) '-- 'YOU 'WIN)))
		(t (append craps (list 'YOUR 'POINT 'IS (say-throw roll)))))))

;;; exercise 5.6 part g
(defun try-for-point (point)
	(let* ((roll (throw-dice))
		(die1 (first roll))
		(die2 (second roll))
		(extra-point (list 'throw die1 'and die2 '--)))
	(cond ((equal point (+ die1 die2))
			(append extra-point (list (say-throw roll) '-- 'YOU 'WIN)))
		((equal (say-throw roll) 7) (append extra-point (list (say-throw roll) '-- 'YOU 'LOSE)))
		(t (append extra-point (list (say-throw roll) '-- 'THROW 'AGAIN))))))
		










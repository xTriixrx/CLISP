;;; This is the file for the craps game
;;; HOW-TO PLAY:

;;; simulates rolling a single die (1-6)
(defun throw-die ()
	(let ((toss (+ 1 (random 6))))
		(cons toss nil)))

;;; simulates rolling dice (1-6 for each)
(defun throw-dice ()
	(let ((die1 (throw-die))
		(die2 (throw-die)))
		(list (car die1) (car die2))))

;;; predicate to determine if roll is a snake-eyes (1-1)
(defun snake-eyes-p (roll)
	(if 
		(and 
			(equal (first roll) 1) 
			(equal (second roll) 1)) 
		t nil))

;;; predicate to determine if roll is a boxcars (6-6)
(defun boxcars-p (roll)
	(if 
		(and 
			(equal (first roll) 6) 
			(equal (second roll) 6)) 
		t nil))

;;; predicate for determining if a roll was an instant win for craps
(defun instant-win-p (roll)
	(if 
		(or 
			(equal (+ (first roll) (second roll)) 7) 
			(equal (+ (first roll) (second roll)) 11)) 
		t nil))

;;; predicate for determining if a roll was an instant loss for craps
(defun instant-loss-p (roll)
	(if 
		(or 
			(equal (+ (first roll) (second roll)) 2) 
			(equal (+ (first roll) (second roll)) 3) 
			(equal (+ (first roll) (second roll)) 12)) 
		t nil))

;;; determines the roll
(defun say-throw (roll)
	(cond ((boxcars-p roll) 'BOXCARS)
		((snake-eyes-p roll) 'SNAKE-EYES)
		(t (+ (first roll) (second roll)))))

;;; main craps playing functionality, if point is made must call try-for-point with value of point.
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

;;; if trying for a point, if you need to throw again make sure to pass the value of the new point you made.
(defun try-for-point (point)
	(let* ((roll (throw-dice))
		(die1 (first roll))
		(die2 (second roll))
		(extra-point (list 'throw die1 'and die2 '--)))
	(cond ((equal point (+ die1 die2))
			(append extra-point (list (say-throw roll) '-- 'YOU 'WIN)))
		((equal (say-throw roll) 7) (append extra-point (list (say-throw roll) '-- 'YOU 'LOSE)))
		(t (append extra-point (list (say-throw roll) '-- 'THROW 'AGAIN))))))
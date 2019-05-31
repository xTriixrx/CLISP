;; Exercise 3.22 part d:
(defun firstp (x y)(if (equal x (first y)) T NIL))

;; Exercise 3.22 part e:
(defun mid-add1 (x)
	(if (and (= (length x) 3) (numberp (second x)))
		(list (first x) (+ (second x) 1) (third x)) NIL))

;; Exercise 3.22 part f:
(defun f-to-c (temperature)
	(if (numberp temperature)
		(/ (* (- 32 temperature) 5) 9) NIL))


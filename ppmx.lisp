;;; PPMX stands for 'Pretty Print Macro eXpansion'. It macroexpands its first argument (unevaluated)
;;; and prints the result. PPMX is not only useful for learning about built-in macros like SETF, it is
;;; also quite handy for debugging macros you write yourself if there is a problem with their expansion.
(defmacro ppmx (form)
	"PPMX -> Pretty Print Macro eXpansion, prints the macro expansion of FORM."
	`(let* ((exp1 (macroexpand-1 ',form))
			(exp (macroexpand exp1))
			(*print-circle* nil))
		(cond ((equal exp exp1)
				(format t "~&Macro expansion:")
				(pprint exp))
			(t (format t "~&First step of expansion:")
				(pprint exp1)
				(format t "~%~%Final expansion:")
				(pprint exp)))
		(format t "~%~%")
		(values)))
				
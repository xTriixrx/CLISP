;;; Exercise 13.1
;;; This function SUBPROP deletes an element from a set stored under a property name. For example, if the symbol ALPHA
;;; has the list (A B C D E) as the value of its FOOPROP property, doing (SUBPROP 'ALPHA 'D 'FOOPROP) should leave (A B C E)
;;; as the value of ALPHA's FOOPROP property.
(defun subprop (sym rmv prop)
	(cond ((equal 'unknown (get sym prop 'unknown)) nil)
		(t (setf (get sym prop) (remove rmv (get sym prop))))))


;;; Exercise 13.2
;;; This function FORGET-MEETING forgets that 2 particular persons have ever met each other. Use SUBPROP in your solution.
;;; REQUIRES RECORD-MEETING AND ADDPROP FUNCTIONS:
(defun addprop (sym elem prop)
	(pushnew elem (get sym prop)))

(defun record-meeting (x y)
	(addprop x y 'has-met)
	(addprop y x 'has-met) t)

(defun forget-meeting (x y)
	(subprop x y 'has-met)
	(subprop y x 'has-met) t)

;;; Exercise 13.3
;;; Using SYMBOL-PLIST, write your own version of the GET function.
;;; STILL NEEDS WORK!!!
;;; This definition does not work with setf such as: (setf (my-get 'fred 'age) 21)
;;; This definition also does not accept an optional third parameter to return a value other than NIL.

;;; THIS VERSION OF MY-GET IS VERY BASIC, INTERATIVE AND ONLY CAN RETURN VALUE OF PROPERTY.
(defun my-get (sym prop)
	(let ((tmp (symbol-plist sym))
		(next nil))
		(dolist (elem tmp nil)
			(if (equal next t) (return elem))
			(when (equal elem prop)
				(setf next t)))))

;;; Exercise 13.4
;;; This function HASPROP returns T or NIL in order to indicate whether a symbol has a particular property, independent of 
;;; the value of that property. Note: If symbol A has a property FOO with value NIL, (HASPROP 'A 'FOO) should still return T.
(defun hasprop (sym prop)
	(if (equal 'unknown (get sym prop 'unknown)) nil t))




;;; KEYBOARD EXERCISE 13.8 Find out how the random number generator in LISP works. Will produce a histogram plot of 200
;;; random values between zero and ten, using an array to keep track of how many times we encounter each value.

;;; Exercise 13.8 Part A
;;; Set up global variable *HIST-ARRAY* that holds the array of counts, and a global variable *TOTAL-POINTS*
;;; that holds the number of points recorded so far.
(setf *HIST-ARRAY* 0)
(setf *TOTAL-POINTS* 0)

;;; Exercise 13.8 Part B
;;; Write a function NEW-HISTOGRAM to initialize these variables appropriately. It should take one input:
;;; the number of bins the histogram is to have.
(defun new-histogram (bins)
	(setf *HIST-ARRAY* (make-array 11 :initial-element 0))
	(setf *TOTAL-POINTS* 0) t)

;;; Exercise 13.8 Part C
;;; Write a function RECORD-VALUE that takes a number as input. If the number is between zero and 10, it should increment
;;; the appropriate element of the array, and also update *TOTAL-POINTS*. If the input is out of range,
;;; RECORD-VALUE should issue an appropriate error message.
(defun record-value (num)
	(cond ((and (< num 11) (> num -1)) 
				(incf (aref *HIST-ARRAY* num) 1)
				(incf *TOTAL-POINTS* 1))
		(t (format t "Value ~S is out of bin range." num))))

;;; Exercise 13.8 Part D
;;; Write a function PRINT-HIST-LINE that takes a vale from zero to ten as input, looks up that value in the array,
;;; and prints the corresponding line of the histogram. To get the numbers to line up in columns properly, you
;;; will need to use the format directives ~2S to display the value and ~3S to display the count. You 
;;; can use DOTIMES in order to print the asterisks.
(defun print-hist-line (bin)
	(let ((val (aref *HIST-ARRAY* bin)))
		(format t "~2S " bin)
		(format t "[~2S] " val)
		(dotimes (i val)
			(format t "*"))
		(format t "~%")))


;;; Exercise 13.8 Part E
;;; Write the function PRINT-HISTOGRAM
(defun print-histogram ()
	(let ((len (length *HIST-ARRAY*)))
		(dotimes (i len)
			(print-hist-line i))))





;;; KEYBOARD EXERCISE 13.9 The purpose of this exercise is to make a program to help solve cryptograms. The cryptogram is
;;; represented as a list of strings with all letters being lowercase. This program will recieve input from the user when
;;; starting to solve the cryptogram. If the user wants to undo a substitution they can do so by inputting undo to the 
;;; prompt. The process ends when the user solves the cryptogram.

(setf crypto-text '("zj ze kljjls jf slapzi envlij pib kl jufwxuj p hffv jupi jf"
	"enlpo pib slafml pvv bfwkj"))

;;; Exercise 13.9 Part A
;;; As the user is solving the cryptogram, we will store the information of the conversion in two hash tables called 
;;; *ENCIPHER-TABLE* and *DECIPHER-TABLE*. We will use *DECIPHER-TABLE* to print out the deciphered cryptogram, and we
;;; need *ENCIPHER-TABLE* to check for two letters being deciphered to the same thing. EX: If P is A, and we then do K is A,
;;; need to look at *ENCIPHER-TABLE*, also if P is deciphered to A, then if we tried deciphering P to E, we look at 
;;; *DECIPHER-TABLE* to tell that P had already been deciphered to A.

(setf *ENCIPHER-TABLE* (make-hash-table))
(setf *DECIPHER-TABLE* (make-hash-table))

;;; Exercise 13.9 Part B
;;; This function MAKE-SUBSTITUTION takes two character objects as input and stores the appropriate entries in 
;;; *DECIPHER-TABLE* and *ENCIPHER-TABLE* so that the first letter deciphers the second and the second letter 
;;; enciphers the first. This function does NOT check if either letter is already has an entry in these hash tables.

(defun make-substitution (ch1 ch2)
	(setf (gethash ch1 *ENCIPHER-TABLE*) ch2)
	(setf (gethash ch2 *DECIPHER-TABLE*) ch1))

;;; Exercise 13.9 Part C
;;; This function UNDO-SUBSTITUTION takes one letter as input and it should set the *DECIPHER-TABLE* entry of that letter,
;;; and the *ENCIPHER-TABLE* entry of the letter it deciphered to, to NIL.
(defun undo-substitution (ch)
	(let ((undo (gethash ch *ENCIPHER-TABLE*)))
		(setf (gethash ch *ENCIPHER-TABLE*) nil)
		(setf (gethash undo *DECIPHER-TABLE*) nil))) 

;;; Exercise 13.9 Part D
;;; Look up the documentation for the CLRHASH function, this function CLEAR clears the two hash tables used in problem.
(defun clear ()
	(clrhash *ENCIPHER-TABLE*)
	(clrhash *DECIPHER-TABLE*))

;;; Exercise 13.9 Part E
;;; This function DECIPHER-STRING takes a single encoded string as input and returns a new, partially decoded string.
;;; It should begin by making a new string of the same length as the input, containing all spaces. In order to do that,
;;; use the following command (make-string len :initial-element #\Space). Next the function should iterate through 
;;; the elements of the input string. For each character that deciphers to something non-NIL, that value should be
;;; inserted into the corresponding position in the new string. When testing make sure all inputs are lowercase.
(defun decipher-string (str)
	(let* ((len (length str))
		(decipher (make-string len :initial-element #\Space))
		(count 0))
		(dotimes (i len decipher)
			(unless (not (equal (gethash (aref str i) *ENCIPHER-TABLE*) nil))
				(incf count 1))
			(when (not (equal (gethash (aref str i) *ENCIPHER-TABLE*) nil))
				(setf (aref decipher count) (gethash (aref str i) *ENCIPHER-TABLE*))
				(incf count 1)))))
				

;;; Exercise 13.9 Part F
;;; This function SHOW-LINE that displays one line of cryptogram text, with the deciphered text displayed beneath it.
(defun show-line (line)
	(format t "~%~S" line)
	(format t "~%~S~%" (decipher-string line)))


;;; Exercise 13.9 Part G
;;; This function SHOW-TEXT takes a cryptogram (list of strings) as input and displays lines like examples.
(defun show-text (cryptogram)
	(dolist (c cryptogram (format t "--------------------~%"))
		(show-line c)))

;;; Exercise 13.9 Part H
;;; This function GET-FIRST-CHAR returns the first character in lowercase printed representation of an object.
(defun get-first-char (x)
	(char-downcase (char (format nil "~A" x) 0)))

;;; Exercise 13.9 Part I
;;; This function READ-LETTER reads an object from the keyboard. If the object is the symbol END or UNDO, it should 
;;; be returned as the value of READ-LETTER. Otherwise, READ-LETTER should use GET-FIRST-CHAR on the object to extract
;;; the first character of its printed representation; should return that character as its result.
(defun read-letter ()
	(let ((input (read)))
		(cond ((equal input 'end) 'end)
			((equal input 'undo) 'undo)
			(t (get-first-char input)))))

;;; Exercise 13.9 Part J
;;; This function SUB-LETTER takes a character object as input, if that character has been deciphered already, SUB-LETTER
;;; should print an error message that tells what the letter has been deciphered to. Otherwise SUB-LETTER should ask
;;; "What does (letter) decipher to?" and read a letter. If the result is a character and it has not yet been enciphered,
;;; SUB-LETTER should call MAKE-SUBSTITUTION to record the substitution. Otherwise an appropriate error message should print
(defun sub-letter (chr)
	(let ((result nil)
		(read_result nil))
		(if (equal (gethash chr *ENCIPHER-TABLE*) nil) (setf result t) (setf result nil))
		(cond ((equal result nil) (format t "~%~S has already been deciphered as ~S!" (string chr) (string (gethash chr *ENCIPHER-TABLE*))))
			((equal result t) (format t "~%What does ~S decipher to?" (string chr)) (setf read_result (read-letter))))
		(cond ((and (equal (typep read_result 'standard-char) t) (equal (gethash read_result *DECIPHER-TABLE*) nil))
			(make-substitution chr read_result))
			((not (equal result nil)) (format t "~%But ~S already deciphers to ~S!" (string read_result) (string (gethash read_result *DECIPHER-TABLE*)))))))

;;; Exercise 13.9 Part K
;;; This function UNDO-LETTER asks "Undo which letter?" and reads in a character. If that character has been deciphered, UNDO-LETTER should call
;;; UNDO-SUBSTITUTION on the letter. Otherwise an appropriate error message should be printed.
(defun undo-letter ()
	(format t "Undo which letter?")
	(let ((chr (read-letter)))
		(cond ((equal (gethash chr *DECIPHER-TABLE*) nil) (undo-substitution chr))
			(t (format t "~%~S hasn't been enciphered yet." (string chr))))))

;;; Exercise 13.9 Part L
;;; This function SOLVE takes a cryptogram as input and performs the following loop. First it should display the cryptogram, then ask "Substitute which letter?"
;;; and call READ-LETTER. If the result is a character, SOLVE should call SUB-LETTER; if the result is symbol UNDO, it should call UNDO-LETTER; if the result 
;;; is the symbol END, it should return T. Otherwise it should issue an error message. Then it should go back to the beginning of the loop unless the value 
;;; returned by the READ-LETTER was END.
(defun solve (cryptogram)
	(format t "--------------------~%")
	(do ((response nil))
		((equal response 'end) t)
		(show-text cryptogram)
		(format t "~%Substitute which letter?")
		(setf response (read-letter))
		(cond ((typep response 'standard-char) (sub-letter response))
			((equal response 'undo) (undo-letter))
			((equal response 'end) t)
			(t (format t "~%Error ~S is an invalid response try again." (string response))))
		(format t "~%--------------------~%")))

;;; Exercise 13.9 Part M
;;; P deciphers to A, and Z deciphers to I, solve the rest of the cryptogram.
;;; IF YOU FEEL LIKE DECIPHERING THE CRYPTOGRAM PLEASE FEEL FREE.

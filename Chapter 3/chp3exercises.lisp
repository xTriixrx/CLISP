;;; Exercise 3.1
;;; Medium
;;; Show a lambda expression that is equivalent to the above let* expression.
;;; HINT: You may need more than one lambda expression.
;;; -> (let* ((x 6) (y (* x x))) (+ x y)) => 42.
(defun lambda-equivalent ()
  "Returns the equivalent of the let* statement except with lambda's."
  ((lambda (x y)
      (+ x y)) 6 ((lambda (x)
                    (* x x)) 6)))

;;; Exercise 3.2
;;; Seconds
;;; The function cons can be seen as a special case of one of the other functions
;;; listed previously. Which one?

;;; ANSWER: -> (cons a b) === (list* a b)

;;; Exercise 3.3
;;; Medium
;;; Write a function that will print an expression in dotted pair notation. Use the
;;; built-in function princ to print each component of the expression.
(defun dotted-notation (lst)
  (cond ((atom lst) (format t "~S" lst))
    (t (format t "(")
       (dotted-notation (car lst))
       (format t " . ")
       (dotted-notation (cdr lst))
       (format t ")"))) nil)


;;; Exercise 3.4
;;; Medium
;;; Write a function that, like the regular print function, will print an expression
;;; in dotted pair notation when necessary but will use normal list notation when
;;; possible.
;;; EX: (A . NIL) -> (A) (A . B) -> (A . B) ((A . NIL) . (B . (C . D))) -> ((A) B C . D)
(defun hybrid-notation (x)
  (labels ((hybrid-cdr (x)
            (cond ((null x)
                   (format t ")"))
                  ((atom x)
                   (format t " . ~s)" x))
                  (t
                    (format t " ")
                    (hybrid-notation (car x))
                    (hybrid-cdr (cdr x))))))
    (cond ((atom x)
           (format t "~s" x))
          (t (format t "(")
             (hybrid-notation (first x))
             (hybrid-cdr (rest x))))) nil)


;;; Exercise 3.5
;;; Hard
;;; (Exercise in altering structure.) Write a program that will play the role of
;;; the guesser in the game Twenty Questions. The user of the program will have
;;; in mind any type of thing. The program will ask questions of the user, which
;;; must be answered yes or no, or "it" when the program has guessed it. If the
;;; program runs out of guesses, it gives up and asks the user what "it" was. At
;;; first the program will not play well, but each time it plays, it will remember
;;; the user's replies and use them for subsequent guesses.

;;; NOTE: I have made a small (but substancial) update to this exercise, rather
;;; than losing all progress after adding new questions and closing the session,
;;; questions can be REMEMBERED by calling the (update-database *db*) and saving
;;; the evaluable expression into a text file. Play around with this example for
;;; a little if you are confused. Feel free to reach out to me if you have any
;;; questions, concerns, or suggestions.

;;; Defines the node structure for the question game
(defstruct (node (:print-function print-node))
  name
  (yes nil)
  (no nil) t)

(defconstant *database* "questions.txt")

;;; Holds the current instance of the database, will be updated during a session
;;; and needs to be pushed back into a text file after a session for saving.
(defvar *db* nil)

;;; Returns a string of all contents of a file
(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;;; Grabs last saved instance of questions database from database.txt file.
(defun grab-database ()
 (let* ((contents (file-get-contents *database*))
        (contents (read-from-string contents)))
   contents))

;;; MACRO, translates the last saved instance into commands necessary for lisp
;;; environment.
(defmacro initialize-database ()
 `(setf *db* ,(grab-database)))

;;; Pushes data to database file and overwrites the existing "table".
(defun push-database (filename content)
  (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (let ((new-content (write-to-string content)))
      (format stream new-content))))

;;; Updates the database file with the latest content from the *db* variable.
;;; THIS FUNCTION NEEDS TO BE CALLED WITH *DB* WHEN FILE WANTS/NEEDS TO BE UPDATED
(defun update-database (content &optional (filename *database*))
  (push-database filename content) t)

;;; This function is the custom printing mechanism for node objects.
;;; This needs to exist because it establishes the relationship between pulling
;;; the database from the text file as well as pushing the updated table.
(defun print-node (x stream depth)
  (cond ((and (node-name x) (not (node-yes x)) (not (node-no x)))
         (format stream "(make-node :name '~S)" (node-name x)))
        ((and (node-name x) (node-yes x) (not (node-no x)))
         (format stream "(make-node :name '~S :yes ~S)"
                 (node-name x) (node-yes x)))
        ((and (node-name x) (not (node-yes x)) (node-no x))
         (format stream "(make-node :name '~S :no ~S)"
                 (node-name x) (node-no x)))
        ((and (node-name x) (node-yes x) (node-no x))
         (format stream "(make-node :name '~S :yes ~S :no ~S)"
                 (node-name x) (node-yes x) (node-no x)))
        (t (format stream "ERROR"))))

;;; This function is the main loop for the questions game. The program will
;;; ask multiple questions and the user must respond either yes or no. When the
;;; computer asks the correct item that the user is thinking, the user must respond
;;; with 'it' to end the loop otherwise the loop will end on the function give up.
(defun questions (&optional (node *db*))
  (format t "~& Is it a ~a? " (node-name node))
  (case (read)
    ((y yes) (if (not (null (node-yes node)))
                 (questions (node-yes node))
                 (setf (node-yes node) (give-up))))
    ((n no) (if (not (null (node-no node)))
                (questions (node-no node))
                (setf (node-no node) (give-up))))
    (it 'aha!)
    (t (format t "Reply with YES, NO, or IT if I have guessed it.")
       (questions node))))

;;; This function is called when the computer has ran out of questions to provide
;;; the user, this will generate a new question within the system.
(defun give-up ()
  (format t "~& I give up - what is it? ")
  (make-node :name (read)))


;;; Exercise 3.6
;;; Seconds
;;; Given the following initalization for the lexical variable a and the special
;;; variable *b*, what will the value of the let form?
;;; (setf a 'global-a)
;;; (defvar *b* 'global-b)
;;; (defun fn () *b*)
;;; (let ((a 'local-a)
;;;       (*b* 'local-b))
;;;   (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

;;; ANSWER: -> (LOCAL-A LOCAL-B LOCAL-B GLOBAL-A LOCAL-B)

;;; Exercise 3.7
;;; Seconds
;;; Why do you think the leftmost of two keys is the one that counts, rather
;;; than the rightmost?
;;; ANSWER -> There are 2 good reasons, first it makes it faster to search through
;;; the argument list: just search until you find the key, not all the way to the
;;; end. Secondly, in the case where you want to override an existing keyword and
;;; pass the argument list on to another function, it is cheaper to cons the new
;;; keyword/value pair on the front of a list than to append it to the end of a list.


;;; Exercise 3.9
;;; Medium
;;; Write a version of length using the function reduce.
(defun reduce-length (lst)
  (reduce #'(lambda (x y) (+ x 1)) lst :initial-value 0))
;;; OR -> (reduce #'+ (mapcar #'(lambda (x) 1) lst))


;;; Exericise 3.10
;;; Medium
;;; Use a reference manual or describe funct to figure out what the functions
;;; LCM and NRECONC do.
;;; ANSWER -> LCM = DEFINED AS LEAST COMMON MULTIPLE
;;;           NRECONC = nreconc reverses the order of elements in list
;;;           (as if by nreverse). It then appends (as if by nconc) the tail to
;;;           that reversed list and returns the result.
;;;           EX: (nreconc '(a b c) '(d e f)) -> (C B A D E F)

;;; Exercise 3.11
;;; Medium
;;; There is a built-in Common Lisp function that, given a key, a value, and an
;;; association list, returns a new association list that is extended to include
;;; the key/value pair. What is the name of this function?
;;; ANSWER -> ACONS

;;; Exercise 3.12
;;; Medium
;;; Write a single expression using format that will take a list of words and
;;; print them as a sentence, with the first word capitalized and a period after
;;; the last word. You will have to consult a reference to learn new format directives.
(defun sentence-builder (lst)
  (format t "~@(~{~a~^ ~}.~)" lst))

;;; KEYBOARD EXERCISE 12.4 Implementation of a discrimination net. (A network of yes and no questions used for
;;; problem solving tasks such as diagnosing automotive engine trouble.

;;; Exercise 12.4 Part A
;;; Write a DEFSTRUCT for a structure called NODE, with four components called NAME, QUESTION, YES-CASE, NO-CASE.
(defstruct node
	(nname nil)
	(question nil)
	(yes-case nil)
	(no-case nil))

;;; Exercise 12.4 Part B
;;; Define a global variable *NODE-LIST* that will hold all the noes in the discrimination net. Write a function
;;; INIT that initializes the network by setting *NODE-LIST* to NIL.
(setf *NODE-LIST* nil)
(defun init () 
	(setf *NODE-LIST* nil))

;;; Exercise 12.4 Part C
;;; Write function ADD-NODE, adds new NODE to *NODE-LIST* and returns the name of the NODE it added to list.
(defun add-node (nme q yes no)
	(setf *NODE-LIST* (cons (make-node :nname nme 
		:question q 
		:yes-case yes 
		:no-case no) *NODE-LIST*)) nme)

;;; Exercise 12.4 Part D
;;; Write function FIND-NODE, which takes a node name as input and returns the node if it appears in *NODE-LIST*
;;; or NIL if it doesn't.
(defun find-node (nme)
	(dolist (elem *NODE-LIST* nil)
		(when (equal (node-nname elem) nme)
			(return elem))))

;;; Exercise 12.4 Part E
;;; Write function PROCESS-NODE, which takes a node name as input. If the node cannot be found, it prints a
;;; message that the node hasn't been defined yet and returns NIL. Otherwise it asks the user the 
;;; question associated with that node, and then returns the node's yes action or no action depending 
;;; on how the user responds.
(defun process-node (nm)
	(let ((elem (find-node nm)))
	(cond ((null elem)
			(format t "Node ~S hasn't been defined yet." nm) nil)
		(t (if (yes-or-no-p (node-question elem)) (node-yes-case elem) (node-no-case elem) )))))

;;; Exercise 12.4 Part F
;;; This function RUN maintains a local variable named CURRENT-NODE whose inital value is START. It loops,
;;; calling PROCESS-NODE to processes the current node, and storing the value returned by PROCESS-NODE back 
;;; into CURRENT-NODE. If the value returned is a string, the function prints the string and stops. If the
;;; value is NIL it also stops.
(defun run ()
	(do* ((current-node 'start)
		(processed (process-node current-node) (process-node current-node)))
		((null processed) nil)
		(when (typep processed 'string)
			(return processed))
		(setf current-node processed)))

;;; Exercise 12.4 Part G
;;; This function is an interative way to add a new node. It prompts 
;;; the user for the node name, question, yes and no actions.
(defun inter-add-node ()
	(let ((nme nil)
		(question nil)
		(yes nil)
		(no nil))
		(format t "What is the name of the node?")
		(setf nme (read))
		(do ((tmp nil))
			((typep tmp 'simple-base-string) (setf question tmp))
			(format t "What is the question of the node?")
			(setf tmp (read)))
		(format t "What is the yes-case of the node?")
		(setf yes (read))
		(format t "What is the no-case of the node?")
		(setf no (read))
		(add-node nme question yes no) nme))

;;; Exercise 12.4 Part H
;;; This adds a new node case into the diagnosing network.
(add-node 'engine-will-run-briefly 
"Does the engine stall when cold but not when it's warm?"
'cold-idle-700-rpm
"Adjust the idle speed.")

;;; Required nodes for Exercise 12.4
(add-node 'start "Does the engine turn over?" 'engine-turns-over 'engine-wont-turn-over)

(add-node 'engine-turns-over
		  "Will the engine run for any period of time?"
		  'engine-will-run-briefly
		  'engine-wont-run)

(add-node 'engine-wont-run
		  "Is there gas in the tank?"
		  'gas-in-tank
		  "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
		  "Do you hear any sound when you turn the key?"
		  'sound-when-turn-key
		  'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
		  "Is the battery voltage low?"
		  "Replace the battery."
		  'battery-voltage-ok)

(add-node 'battery-voltage-ok
		  "Are the battery cables dirty or loose?"
		  "Clean the cables and tighten the connections."
		  'battery-cables-good)


;;; Exercise 12.5
;;; Creates a structure called CAPTAIN with fields NAME, AGE, and SHIP

(defstruct starship
(captain nil)
(nname nil)
(speed 0)
(condition 'green)
(shields 'down))

(defun print-captain (x stream depth)
	(format stream "#<CAPTAIN \"~A>\"" (captain-nname x)))

(defstruct (captain (:print-function print-captain))
(nname nil)
(age 0)
(ship nil))

(setf s1 '#s(starship nname "Enterprise" speed (warp 5) condition 'red shields 'up))

(setf c1 '#s(captain nname "James T. Kirk" age 35 ship s1))

(setf (starship-captain s1) c1)

(format t "This is ~S speaking." c1)


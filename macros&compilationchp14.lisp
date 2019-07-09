;;; Exercise 14.3
;;; Write a SET-NIL macro that sets a variable to nil.
(defmacro nullify (var)
	(list 'setq var nil))

;;; EXAMPLE MACRO FROM TEXTBOOK
;;; This macro shows how to use the comma in a backquoted list to "unquote" meaning the value of the expression rather than the expression itself.
(defmacro simple-incf (var &optional (amount 1))
	`(setq ,var (+ ,var ,amount)))
	
;;; Exercise 14.4
;;; Write a macro called SIMPLE-ROTATEF that switches the value of two variables. For example, if A is two and B is seven, then (SIMPLE-ROTATEF A B)
;;; should make A seven and B two. Obviously, setting A to B first and setting B to A won't work. Your macro should expand into a LET expression that holds
;;; on to the original values of the two variables and then assigns them their new values in its body.
(defmacro simple-rotatef (a b)
	`(let ((tmp ,a))
		(setq a ,b)
		(setq b tmp) nil))

;;; Exercise 14.5
;;; Write a macro SET-MUTUAL that takes two variable names as input and expands into an expression that sets each variable to the name of the other. (SET-MUTUAL A B)
;;; should set A to 'B, and B to 'A.
(defmacro set-mutual (a b)
	`(let ((tmp '',a))
		(setq ,a '',b)
		(setq ,b tmp) nil))

;;; EXAMPLE MACRO FROM TEXTBOOK
;;; This macro shows how to use the splicing functionality with the backquote and @ character (,@)
(defmacro set-zero (&rest variables)
	`(progn ,@(mapcar #'(lambda (var)
					(list 'setf var 0))
					variables)
			'(zeroed ,@variables)))

;;; Exercise 14.6
;;; Write a macro called VARIABLE-CHAIN that accepts any number of inputs. The expression (VARIABLE-CHAIN A B C D) should expand into an expression that sets
;;; A to 'B, B to 'C and C to 'D. EXTRA FEATURE: -> (VARIABLE-CHAIN A B C D) -> A to 'B, B to 'C, C to 'D and D to 'A!
(defmacro variable-chain (&rest variables)
	`(progn ,@(do* ((vars variables (rest vars))
					(start (first vars))
					(v1 (first vars) (first vars))
					(v2 (second vars) (second vars))
					(chain nil))
				((null v2) (push `(setf ,v1 '',start) chain) (reverse chain))
				(push `(setf ,v1 '',v2) chain))))




;;; CASE STUDY: FINITE STATE MACHINES
;;; This case study will create a general purpose simulator for a finite state machine but will focus on a particular FSM, such as a vending machine.

;;; Variables to keep track of current node, all nodes and all arcs for FSM.
(defvar *NODES*)
(defvar *ARCS*)
(defvar *CURRENT-NODE*)

;;; Initializes state machine
(defun initialize ()
	(setf *NODES* nil)
	(setf *ARCS* nil)
	(setf *CURRENT-NODE* nil))

;;; Special printing function for printing a node.
(defun print-node (node stream depth)
	(format stream "#<Node ~A>"
			(node-nodename node)))

;;; Structure for what a node data-type contains
(defstruct (node (:print-function print-node))
	(nodename nil)
	(inputs nil)
	(outputs nil))

;;; Special printing function for printing an arc.
(defun print-arc (arc stream depth)
	(format stream "#<ARC ~A / ~A / ~A>"
			(node-nodename (arc-from arc))
			(arc-label arc)
			(node-nodename (arc-to arc))))

;;; Structure for what am arc data-type contains
(defstruct (arc (:print-function print-arc))
	(from nil)
	(to nil)
	(label nil)
	(action nil))

;;; Adds a new node to the list *NODES* which is a global variable. Uses NCONC in order to add the node to the end of the list, reasurring that the nodes in 
;;; *NODES* will appear in the order in which they were defined with DEFNODE.
(defun add-node (newname)
	(let ((new-node (make-node :nodename newname)))
		(setf *NODES* (nconc *NODES* (list new-node)))
		new-node))

;;; Syntactic sugar for defining a node.
(defmacro defnode (nname)
	`(add-node ',nname))

;;; This function FIND-NODE takes a node name as input and returns the corresponding node. If no node exists with that name, FIND-NODE signals an error.
(defun find-node (nname)
	(or (find nname *NODES* :key #'node-nodename)
		(error "No node named ~A exists." nname)))

;;; This function ADD-ARC adds an arc to the *ARCS* list which is a global variable. Uses NCONC in order to add the node to the end of the list, reasurring
;;; that the arcs in *ARCS* will appear in the order in wich they were defined with DEFARC.
(defun add-arc (from-name label to-name action)
	(let* ((from (find-node from-name))
			(to (find-node to-name))
			(new-arc (make-arc :from from
							   :label label
							   :to to
							   :action action)))
		(setf *ARCS* (nconc *ARCS* (list new-arc)))
		(setf (node-outputs from)
			  (nconc (node-outputs from)
					 (list new-arc)))
		(setf (node-inputs to)
			  (nconc (node-inputs to)
					 (list new-arc)))
		new-arc))

;;; Syntactic sugar for defining an arc.
(defmacro defarc (from label to &optional action)
	`(add-arc ',from ',label ',to ',action))

;;; This function ONE-TRANSITION prompts for an input and makes the appropriate state transition by changing the value of *CURRENT-NODE*.
;;; If there is no legal transition from the current state given that input, it prints an error message and prompts for input again.
(defun one-transition ()
	(format t "~&State ~A. Input: " 
			(node-nodename *CURRENT-NODE*))
	(let* ((ans (read))
		   (arc (find ans
					  (node-outputs *CURRENT-NODE*)
					  :key #'arc-label)))
		(unless arc
			(format t "~&No arc from ~A has label ~A.~%"
					(node-nodename *CURRENT-NODE*) ans)
			(return-from one-transition nil))
		(let ((new (arc-to arc)))
			(format t "~&~A" (arc-action arc))
			(setf *CURRENT-NODE* new))))

;;; This function FSM takes an optional input specifying the initial state of the machine. FSM repeatedly calls the function ONE-TRANSITION to move
;;; to the next state. When the machine reaches a state with no output arcs it stops.
(defun fsm (&optional (starting-point 'start))
	(setf *CURRENT-NODE* (find-node starting-point))
	(do ()
		((null (node-outputs *CURRENT-NODE*)))
		(one-transition)))

;;; Exercise 14.7
;;; Extend the vending machine example to sell chocolate bars for 25 cents. Make it accept quarters as well as nickels and dimes. When
;;; you put in a quarter it should go "Ker-chunk!"
(initialize)

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode have-25)
(defnode end)

(defarc start nickel have-5 "Clunk!")
(defarc start dime have-10 "Clink!")
(defarc start quarter have-25 "Ker-chunk!")
(defarc start coin-return start "Nothing to return.")

(defarc have-5 nickel have-10 "Clunk!")
(defarc have-5 dime have-15 "Clink!")
(defarc have-5 quarter have-25 "Nickel change.")
(defarc have-5 coin-return start "Returned five cents.")

(defarc have-10 nickel have-15 "Clunk!")
(defarc have-10 dime have-20 "Clink!")
(defarc have-10 quarter have-25 "Dime change.")
(defarc have-10 coin-return start "Returned ten cents.")

(defarc have-15 nickel have-20 "Clunk!")
(defarc have-15 dime have-25 "Clink!")
(defarc have-15 quarter have-25 "Dime and nickel change.")
(defarc have-15 gum-button end "Deliver gum.")
(defarc have-15 coin-return start "Returned fifteen cents.")

(defarc have-20 nickel have-25 "Clunk!")
(defarc have-20 dime have-25 "Nickel change.")
(defarc have-20 gum-button end "Deliver gum, nickel change.")
(defarc have-20 mint-button end "Deliver mints.")
(defarc have-20 coin-return start "Returned twenty cents.")

(defarc have-25 nickel have-25 "Nickel returned.")
(defarc have-25 dime have-25 "Dime returned.")
(defarc have-25 quarter have-25 "Quarter returned.")
(defarc have-25 gum-button end "Deliver gum, dime change.")
(defarc have-25 mint-button end "Deliver mints, nickel change.")
(defarc have-25 chocolate-bar end "Deliver chocolate bar.")
(defarc have-25 coin-return start "Returned twenty-five cents.")

;;; KEYBOARD EXERCISE 14.11 This keyboard exercise is to speed up the interpreter the node case study.

;;; Exercise 14.11 Part A
;;; This function COMPILE-ARC takes an arc as input and returns a COND clause. To test this function, (COMPILE-ARC (FIRST *ARCS*)) should return this list:
;;; ((equal this-input 'nickel) (format t "~&~A" "Clunk!") (have-5 (rest input-syms)))
(defun compile-arc (arc)
  `((equal this-input ',(arc-label arc))
    (format t "~&~A" ,(arc-action arc))
    (,(node-nodename (arc-to arc)) (rest input-syms))))

;;; Exercise 14.11 Part B
;;; This function COMPILE-NODE takes a node as input and returns a DEFUN expression for that node. (COMPILE-NODE (FIND-NODE 'START)) should return the
;;; DEFUN shown previously.
(defun compile-node (node)
  (let ((cond-clauses (mapcar #'compile-arc (node-outputs node))))
    `(defun ,(node-nodename node) (input-syms &aux (this-input (first input-syms)))
       (cond ((null input-syms) ',(node-nodename node))
             ,@cond-clauses
             (t (error "No arc from ~A with label ~A."
                       ',(node-nodename node) this-input))))))

;;; Exercise 14.11 Part C
;;; This macro COMPILE-MACHINE expands into a PROGN containing a defun for each node in *NODES*.
(defmacro compile-machine ()
  `(progn ,@(mapcar #'compile-node *nodes*)))

;;; Exercise 14.11 Part D
;;; What does the expression (start '(dime dime dime gum-button)) produce?
(compile-machine)
(start '(dime dime dime gum-button))

;;; This file is for the game tic tac toe, described in the Common Lisp: A Gentle Introduction 
;;; to Symbolic Computation.

;;; This global variable defines all the possible combinations of winning a game of tic-tac-toe,
;;; where each number represents the position in the list where a marker must be placed.
(setf *triplets* '((1 2 3) (4 5 6) (7 8 9)
				   (1 4 7) (2 5 8) (3 6 9)
				   (1 5 9) (3 5 7)))

(setf *diagonals* '((1 5 9) (3 5 7)))

(setf *corners* '(1 3 7 9))

(setf *sides* '(2 4 6 8))
;;; This function generates the board for the initialization of the game.
;;; Board is represented as a list consisting of symbol BOARD and 9 numbers describing 
;;; content of the board. 0 means empty, 1 means O, 10 means X.
(defun make-board ()
	(list 'board 0 0 0 0 0 0 0 0 0))

;;; These global variables set the players for the tic-tac-toe match.
(setf *PLAYER* 1)
(setf *COMPUTER* 10)
(setf *BOARD* (make-board))

(defun reset-board ()
	(setf *BOARD* (make-board)))

;;; This function converts a zero, one, or ten to a space, O, or an X, respectively.
;;; This function is called by PRINT-ROW, which is then called by PRINT-BOARD.
(defun convert-to-letter (val)
	(cond ((equal val 1) "X")
		((equal val 10) "O")
		(t " ")))

;;; This function prints out exactly one row of the board, and is called by PRINT-BOARD.
(defun print-row (x y z)
	(format t "~&	~A | ~A | ~A"
		(convert-to-letter x)
		(convert-to-letter y)
		(convert-to-letter z)))

;;; This function prints out the current status of the board.
(defun print-board (board)
	(format t "~%")
	(print-row (nth 1 board) (nth 2 board) (nth 3 board))
	(format t "~&       -----------")
	(print-row (nth 4 board) (nth 5 board) (nth 6 board))
	(format t "~&       -----------")
	(print-row (nth 7 board) (nth 8 board) (nth 9 board))
	(format t "~%~%"))

;;; This function destructively changes one of the board positions made by a user.
;;; The player symbol will be either a 1 or 10 depending on whos going.
(defun make-move (player pos board)
	(setf (nth pos board) player) board)

;;; This function returns the sum of the numbers in the board positions specified by that triplet.
;;; EX: For the diagonal triplet (3 5 7), the sum of these elements if lets say was 11, would represent
;;; there is one O, one X, and a blank (unspecified order) on that diagonal.
(defun sum-triplet (board triplet)
	(+ (nth (first triplet) board)
	   (nth (second triplet) board)
	   (nth (third triplet) board)))

;;; This function allows the computation of determining whether or not a winner exists or not.
;;; A winner can be determined if one of the sums in the list equals either 3 (PLAYER WINS) or 30
;;; (COMPUTER WINS).
(defun compute-sums (board)
	(mapcar #'(lambda (triplet)
				(sum-triplet board triplet))
				*triplets*))

;;; This predicate function determines whether or not the player or computer has won the game.
(defun winner-p (board)
	(let ((sums (compute-sums board)))
		(or (member (* 3 *COMPUTER*) sums)
			(member (* 3 *PLAYER*) sums))))

;;; This predicate function determine whether or not the player or computer has won the game.
(defun board-full-p (board)
	(not (member 0 board)))

;;; This function is called by the computer in order to determine the best course of action on
;;; the next play or move. Currently, the best option is to either make 3 in a row or block the player
;;; from winning, however better strategies can be made.
;;; STILL NEEDS ADDING TO BE DONE.
(defun choose-best-move (board)
	(or (make-three-in-a-row board)
		(block-opponent-win board)
		(block-squeeze-play board)
		(block-two-on-one board)
		(random-move-strategy board)))

;;; This random move-strategy is made when no other strategy is needed.
(defun random-move-strategy (board)
	(list (pick-random-empty-position board) "Random Move"))

;;; This picks a random number between 1 and 9 and determines if that
;;; position is empty, if its empty then that position is returned otherwise
;;; this function recursively calls itself to try again.
(defun pick-random-empty-position (board)
	(let ((pos (+ 1 (random 9))))
	(if (zerop (nth pos board))
		pos (pick-random-empty-position board))))

;;; Returns the position of the open position of a triplet if the computer
;;; has 2 positions marked and needs one more to win. If a winning position is 
;;; already filled by the player, the triplet does not count. If no triplets are
;;; found, then the function will return nil, resulting in the next strategy to be tested.
(defun make-three-in-a-row (board)
	(let ((pos (win-or-block board (* 2 *COMPUTER*))))
	(and pos (list pos "Make 3 in a row."))))

;;; Returns the position of the open position of a triplet if the player has 2 position
;;; and needs one more to win. In this case, if the computer notices the player can win,
;;; it will block the player. Otherwise, if their are not triplets that need blocking or
;;; cannot be blocked, the function will return nil in order to test the next strategy.
(defun block-opponent-win (board)
	(let ((pos (win-or-block board (* 2 *PLAYER*))))
	(and pos (list pos "Block Player."))))

(defun block-squeeze-play (board)
	(let* ((triplet (find-if #'(lambda (trip)
								(and (equal (sum-triplet board trip) (+ (* 2 *PLAYER*) *COMPUTER*))
									(equal *COMPUTER* (nth (second trip) board)))) *diagonals*))
		(pos (when triplet (find-empty-position board *sides*))))
	(and pos (list pos "Block Squeeze Play."))))


(defun block-two-on-one (board)
	(let* ((triplet (find-if #'(lambda (trip)
								(and (equal (sum-triplet board trip) (+ (* 2 *PLAYER*) *COMPUTER*))
									(or (equal *COMPUTER* (nth (first trip) board))
										(equal *COMPUTER* (nth (third trip) board))))) *diagonals*))
		(pos (when triplet (find-empty-position board *corners*))))
	(and pos (list pos "Block Two On One Play."))))

;;; Tests whether or not a winning or blocking triplet exists, finds all triplets that
;;; match the passed target-sum. If triplets are found the triplets will then find the first
;;; empty position where the computer can make a move at.
(defun win-or-block (board target-sum)
	(let ((triplet (find-if #'(lambda (trip)
								(equal (sum-triplet board trip) target-sum)) *triplets*)))
	(when triplet (find-empty-position board triplet))))

;;; This function finds an empty position based on the triplets found in the system.
(defun find-empty-position (board squares)
	(find-if #'(lambda (pos)
					(zerop (nth pos board))) squares))

;;; This function controls the computer placement, decision making, and the formatting of
;;; the game play. Also checks the game ending conditions that can stop the game loop.
(defun computer-move (board)
	(let* ((best-move (choose-best-move board))
		   (pos (first best-move))
		   (strategy (second best-move))
		   (new-board (make-move *COMPUTER* pos board)))
	(format t "~&My move: ~S" pos)
	(format t "~&My strategy: ~A~%" strategy)
	(print-board new-board)
	(cond ((winner-p new-board) (format t "~&I Win!") (reset-board))
		((board-full-p new-board) (format t "~&Tie game.") (reset-board))
		(t (player-move new-board)))))

;;; This function validates the user input of a move and continues to present
;;; the user with input until the user has given correct input. Checks for if input
;;; is invalid or already occupied.
(defun read-a-legal-move (board)
	(format t "~&Your move: ")
	(let ((pos (read)))
	(cond ((not (and (integerp pos) (<= 1 pos 9)))
			(format t "~&Invalid input.")
			(read-a-legal-move board))
		((not (zerop (nth pos board)))
			(format t "~&That space is already occupied.")
			(read-a-legal-move board))
		(t pos))))

;;; This function controls the placement and user input from the player. Once a valid
;;; move has been given, the game loop is updated and checks for any game ending conditions
;;; are satisfied in order to end the game.
(defun player-move (board)
	(let* ((pos (read-a-legal-move board))
			(new-board (make-move *PLAYER* pos board)))
	(print-board new-board)
	(cond ((winner-p new-board) (format t "~&You Win!") (reset-board))
		((board-full-p new-board) (format t "~&Tie game.") (reset-board))
		(t (computer-move new-board)))))

;;; This function initiates the initial game loop and allows the player to either have the 
;;; first move or have the computer place its move first.
(defun play-one-game ()
	(if (y-or-n-p "Would you like to go first? ")
		(player-move *BOARD*)
		(computer-move *BOARD*)))























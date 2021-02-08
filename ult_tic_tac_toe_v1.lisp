;;;; Ultimate Tic-Tac-Toe




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLE DECLARATIONS;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *endRound*)     ;;;Indicates when the game is over
(defvar *winner*)       ;;;Indicates who won the game
(defvar *player*)       ;;;Indicates which players turn it is
;;(defvar *turnCount*)    ;;;Keeps track of game turn count, up to 9 turns
;;(defvar *gameArray*)    ;;;Holds game board data
;;(defvar i)              ;;;loop count variable



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GAME FLOW CONTROL FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun game ()            ;;;Starts the game
  (let ((turnCount 0) (gameArray (makeBoard)) (coords (0 0)) )
    
    (printBoard gameArray "GAME")            ;;;prints the board array

    (loop

      
      (makeMove gameArray player winner endRound)                               ;;;Current player makes their move
      (printBoard (gameArray))                             ;;;Board printed after player move
      (setf turnCount (+ turnCount 1))     ;;;Increments the turn count by 1
      (checkWin (gameArray))                               ;;;Checks if a win condition is met
      
      (if (= turnCount 9)                    ;;;if game board is full, declare a tie
          (progn (setf *winner* "draw")
                 (setf *endRound* t))                       
          )
      
      (if (eq *endRound* nil)
          (switchPlayer)                      ;;;Switches between players every turn if game is not over
          )
      
      (when (not (string= *endRound* nil))  (return))     ;;;Exits the game loop when win condition met
      )
    (showResults)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWITCH PLAYER FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switchPlayer ()
  (if (string= *player* "X")          ;;;If previous player is X, change to player O
      (setq *player* "O")
      (setq *player* "X"))            ;;;else, set player to X
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETURNS A 3x3 ARRAY OF ALL SPACES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makeBoard ()
  (let ((blankArray (make-array '(3 3) 
                                :initial-contents '((" " " " " ") (" " " " " ") (" " " " " ")))))
    (return-from makeBoard blankArray)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUTS A 3x3 GAME ARRAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printBoard (arrayToPrint gameType)
  (format t "~a BOARD:~%" gameType)
  (dotimes (i 3)
    (format t "|")
    (format t (aref arrayToPrint i 0))
    (format t "|")
    (format t (aref arrayToPrint i 1))
    (format t "|")
    (format t (aref arrayToPrint i 2))
    (format t "|")
    (terpri)
    )
  (format t "~%")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PLAYER MAKES THEIR MOVE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makeMove (player winner endRound)
  (let ((move " "))
    (format t "KEY:~%")
    (format t "1|2|3~%4|5|6~%7|8|9~%")
    (format t "(O to quit)~%")
    (format t "Player ~a Enter your move:" player)
    (setq move (read))
    (format t "~%")

    (case move
      (0 (progn
           (format t "Quitting Game~%")
           (setq winner "quit")
           (setq endRound t)))
      
      (1 (progn
           (format t "You chose to play at square 1 (0,0)~%")
           (checkValidMove 0 0 currentArray player)))

      (2 (progn
           (format t "You chose to play at square 2 (0,1)~%")
           (checkValidMove 0 1 currentArray player)))

      (3 (progn     
           (format t "You chose to play at square 3 (0,2)~%")
           (checkValidMove 0 2 currentArray player)))

      (4 (progn
           (format t "You chose to play at square 4 (1,0)~%")
           (checkValidMove 1 0 currentArray player)))

      (5 (progn
           (format t "You chose to play at square 5 (1,1)~%")
           (checkValidMove 1 1 currentArray player)))

      (6 (progn
           (format t "You chose to play at square 6 (1,2)~%")
           (checkValidMove 1 2 currentArray player)))

      (7 (progn
           (format t "You chose to play at square 7 (2,0)~%")
           (checkValidMove 2 0 currentArray player)))

      (8 (progn
           (format t "You chose to play at square 8 (2,1)~%")
           (checkValidMove 2 1 currentArray player)))

      (9 (progn
           (format t "You chose to play at square 9 (2,2)~%")
           (checkValidMove 2 2 currentArray player)))

      (otherwise (progn
                   (format t "ERRROR: Invalid Selection, please choose again~%")
                   (printBoard currentArray "GAME")
                   (makeMove (player winner endRound)))
       )
     )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VALIDATES PLAYER MOVE, CHANGES ARRAY IF VALID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun checkValidMove (xCoord yCoord currentArray player)
  (if (or (string= (aref currentArray xCoord yCoord) "X") (string= (aref currentArray xCoord yCoord) "O"))  ;;;if there is already a symbol in the selected space, the player must choose again
      (progn
        (format t "ERRROR: Space already Occupied, Pick again~%")
        (printBoard (currentArray))
        (makeMove (currentArray)))
      (setf (aref currentArray xCoord yCoord) player)                                                    ;;;else, sets player choice to their symbol
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHECKS FOR GAME WIN CONDITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun checkWin ()
  (checkHoriz)
  (checkVert)
  (checkDiag)

)

(defun checkHoriz ()
  (setf i 0)
  
  (dotimes (i 3)
    (if (and (string= (aref *gameArray* i 0) *player*) (string= (aref *gameArray* i 1) *player*) (string= (aref *gameArray* i 2) *player*))
        (setq *endRound* t)
    )
  )
)

(defun checkVert ()
  (setf i 0)
  
  (dotimes (i 3)
    (if (and (string= (aref *gameArray* 0 i) *player*) (string= (aref *gameArray* 1 i) *player*) (string= (aref *gameArray* 2 i) *player*))
        (setq *endRound* t)
    )
  )
)

(defun checkDiag ()
  (if (and (string= (aref *gameArray* 0 0) *player*) (string= (aref *gameArray* 1 1) *player*) (string= (aref *gameArray* 2 2) *player*))
      (setq *endRound* t)
  )
  
  (if (and (string= (aref *gameArray* 2 0) *player*) (string= (aref *gameArray* 1 1) *player*) (string= (aref *gameArray* 0 2) *player*))
      (setq *endRound* t)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION TO DECLARE WINNER OR ANNOUNCE A DRAW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun showResults ()
  (if (string= *winner* "draw")
      (progn (format t "Game is a tie, try again!~%")
             (game))
      (if (string= *winner* "quit")
          (format t "Exiting Game")
          (format t "Player ~a Wins!~%" *player*)
      )
  )
  )



======================
from scratch buffer

(defvar *quit* nil)   ;;;bool used to indicate the game should end

(defun main ()
  (let ((gameArray (makeBoard))
        (player "X")
        (selection 0)
        (win nil)
        (tie nil)
        (turnCount 0))
    
    (setf *quit* nil)
    (loop
      (printBoard gameArray "GAME")                                      ;;;Print board
      
      (setq selection (selectMove gameArray player))                     ;;;select move fcn returns player selection in form of a list
      
      (if (eq *quit* nil)
          (progn
            ;;(format t "point 2~%")
            (setq gameArray (checkValidSpace selection gameArray player))  ;;;checkvalidspce updates and returns array if valid 

            )  
          )
      
      (printBoard gameArray "GAME")                                      ;;;print board

      (setf turnCount (+ turnCount 1))                                   ;;;Increments the turn count by 1
      
      (setf win (checkWin gameArray player))                             ;;;Check win returns true if win condition met
      (if (eq win t)
          (setf *quit* t)
          )
      
      (if (= turnCount 9)                                                ;;;if game board is full, declare a tie and set quit to true
          (progn
            (setf tie t)
            (setf *quit* t)
            )                       
          )
      (setf player (switchPlayer player))
      
      (when (eq *quit* t)  (return))                                         ;;;Exits the game loop when win condition met
      )
    (roundOver player tie)
    )
  )


(defun roundOver (player tie)
  (format t "ROUND OVER~%")
  (if (eq tie t)
      (progn
        (format t "NOBODY WINS, PLAY AGAIN~%")
        (main)
        )
      (format t "PLAYER ~a WINS THIS ROUND!~%" player)
      )
  )


(defun switchPlayer (player)
  (if (string= player "X")          ;;;If previous player is X, change to player O
      (setq player "O")
      (setq player "X")             ;;;else, set player to X
      )
  )

(defun makeBoard ()
  (let ((blankArray (make-array '(3 3) 
                                :initial-contents '((" " " " " ") (" " " " " ") (" " " " " ")))))
    (return-from makeBoard blankArray)
    )
  )

(defun printBoard (arrayToPrint gameType)
  (format t "~a BOARD:~%" gameType)
  (dotimes (i 3)
    (format t "|")
    (format t (aref arrayToPrint i 0))
    (format t "|")
    (format t (aref arrayToPrint i 1))
    (format t "|")
    (format t (aref arrayToPrint i 2))
    (format t "|")
    (terpri)
    )
  (format t "~%")
  )


(defun printKey (player)
  (format t "KEY:~%")
  (format t "1|2|3~%4|5|6~%7|8|9~%")
  (format t "(O to quit)~%")
  (format t "Player ~a Enter your move:" player)
  )


(defun selectMove (currentArray player)
  (let ((selection " "))

    (printKey player)
    (setq selection (read))
    (format t "~%")

    (case selection
      (0 (progn
           (format t "Quitting Game~%")
           (setf *quit* t)))      
      (1 (progn
           (format t "You chose to play at square 1 (0,0)~%")
           (return-from selectMove '(0 0))))
      (2 (progn
           (format t "You chose to play at square 2 (0,1)~%")
           (return-from selectMove '(0 1))))
      (3 (progn     
           (format t "You chose to play at square 3 (0,2)~%")
           (return-from selectMove '(0 2))))
      (4 (progn
           (format t "You chose to play at square 4 (1,0)~%")
           (return-from selectMove '(1 0))))
      (5 (progn
           (format t "You chose to play at square 5 (1,1)~%")
           (return-from selectMove '(1 1))))
      (6 (progn
           (format t "You chose to play at square 6 (1,2)~%")
           (return-from selectMove '(1 2))))
      (7 (progn
           (format t "You chose to play at square 7 (2,0)~%")
           (return-from selectMove '(2 0))))
      (8 (progn
           (format t "You chose to play at square 8 (2,1)~%")
           (return-from selectMove '(2 1))))
      (9 (progn
           (format t "You chose to play at square 9 (2,2)~%")
           (return-from selectMove '(2 2))))
      (otherwise (progn
                   (format t "ERRROR: Invalid Selection, please choose again~%")
                   (printBoard currentArray "GAME")
                   (selectMove currentArray player))
       )
      )
    )
  )


(defun checkValidSpace (selection arrayToCheck player)
  (let ((xCoord 0) (yCoord 0))
    (setf xCoord (car selection))
    (setf yCoord (car (cdr selection)))
    ;;(format t "X:~a~%" xCoord)
    ;;(format t "Y:~a~%" yCoord)

    (if (not (string= (aref arrayToCheck xCoord yCoord) " "))               ;;player must choose empty space
        (progn
          (format t "ERRROR: Space already Occupied, Pick again~%")
          (printBoard arrayToCheck "GAME")
          (selectMove arrayToCheck player)
          )
        (progn
          (setf (aref arrayToCheck xCoord yCoord) player)                     ;;;else, sets player choice to their symbol
          (return-from checkValidSpace arrayToCheck)
          )    
        )  
    )
  )


(defun checkWin (arrayToCheck player)
  (if (or (eq (checkHoriz arrayToCheck player) t) 
          (eq (checkVert arrayToCheck player) t)
          (eq (checkDiag arrayToCheck player) t))
      (progn ;;(write "should be win")
        (return-from checkWin t))
      
      )
  )

(defun checkHoriz (arrayToCheck player)
  (setf i 0)
  
  (dotimes (i 3)
    (if (and (string= (aref arrayToCheck i 0) player) (string= (aref arrayToCheck i 1) player) (string= (aref arrayToCheck i 2) player))
        (return-from checkHoriz t)
        )
    )
  )

(defun checkVert (arrayToCheck player)
  (setf i 0)
  
  (dotimes (i 3)
    (if (and (string= (aref arrayToCheck 0 i) player) (string= (aref arrayToCheck 1 i) player) (string= (aref arrayToCheck 2 i) player))
        (return-from checkVert t)
        )
    )
  )

(defun checkDiag (arrayToCheck player)
  (if (and (string= (aref arrayToCheck 0 0) player) (string= (aref arrayToCheck 1 1) player) (string= (aref arrayToCheck 2 2) player))
      (return-from checkDiag t)
      )
  
  (if (and (string= (aref arrayToCheck 2 0) player) (string= (aref arrayToCheck 1 1) player) (string= (aref arrayToCheck 0 2) player))
      (return-from checkDiag t)
      )
  )

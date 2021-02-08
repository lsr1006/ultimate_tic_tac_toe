;;;; ULTIMATE TIC-TAC-TOE GAME


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLE DECLARATIONS;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *endRound* nil)   ;;;bool used to indicate the round should end
(defvar *quitGame* nil)   ;;;Bool used to indicate the game should quit immediately without declaring a winner or tie



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULTIMATE GAME FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ultGame ()

  ;;;INTRODUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (format t "Welcome to ULTIMATE TIC-TAC-TOE~%~%")
  
  (format t "INSTRUCTIONS:~%")
  (format t "- The goal is to win Tic-Tac-Toe on the ULTIMATE game board.~%")
  (format t "- In order to place your symbol on the ultimate board, you must first win a round of regular Tic-Tac-Toe.~%")
  (format t "- Rounds resulting in a tie will be replayed until someone wins~%")
  (format t "- Play up to 9 rounds of Tic-Tac-Toe to declare the ultimate winner!~%")
  ;;;END-INTRODUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((roundNum 0)                   ;;Keeps track of rounds played
        (roundWinner " ")              ;;Sets winner to nobody by default
        (ultArray (makeBoard))         ;;Initializes ult game array with all spaces
        (win nil)                      ;;Sets default win condition to nil
        (tie nil)                      ;;Sets default tie condition to nil
        (gameType "ULTIMATE"))         ;;Printed out when game board is printed
  ;;;END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setf *endRound* nil)                                                          ;;;Ensures *endRound* and *quitGame* are set to nil at the start of each round
    (setf *quitGame* nil)
    
    (loop
      (format t "~%~%ROUND ~a~%" (+ roundNum 1))
      (setf roundWinner (subGame))
      
      (if (eq *quitGame* nil)                                                      ;;;If player decides to quit, remainder of ultGame loop will be skipped
          (progn
            (format t "~%~%PLAYER ~a WINS ROUND ~a! CHOOSE YOUR SPOT ON THE ULTIMATE BOARD.~%" roundWinner roundNum)
            (printBoard ultArray gameType)
            (setq ultArray (selectMove ultArray roundWinner gameType))             ;;;select move fcn returns array after player selection is made
            
            (setf roundNum (+ roundNum 1))                                         ;;;Increments the turn count by 1
            
            (setf win (checkWin ultArray roundWinner))                             ;;;Check win returns true if win condition met
            
            (if (eq win t)                                                         ;;;If someone wins, end the round
                (setf *endRound* t)
            )
            
            (if (and (= roundNum 9) (eq win nil))                                  ;;;if game board is full, declare a tie and end the round
                (progn
                  (setf tie t)
                  (setf *endRound* t)
                )                       
            )
          )
      )

      (when (or (eq *endRound* t) (eq *quitGame* t))  (return))                    ;;;Exits the loop if *endRound* or *quitGame* are true
      );;;end of Loop
    
    (gameOver tie roundWinner)                                                     ;;;gameOver function declares that the game has ended or restarts the entire game in the case of a tie
  );;;end of LET
);;;end of ultGame function



;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUB GAME FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subGame ()
  
  ;;;LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;
  (let ((gameArray (makeBoard))     ;;Initializes game array to all spaces
        (player "X")                ;;Sets first player to X
        (win nil)                   ;;Sets default win condition to nil
        (tie nil)                   ;;Sets default tie condition to nil
        (turnCount 0)               ;;Keeps track of turns made in each round
        (gameType "GAME"))          ;;Printed out when game board is printed
  ;;;END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;
    
    (setf *endRound* nil)                                                ;;;Ensures *endRound* and *quitGame* are set to nil at the start of each round
    (setf *quitGame* nil)
    (printBoard gameArray gameType)                                      ;;;Print board
    (loop
      
      (setq gameArray (selectMove gameArray player gameType))            ;;;select move fcn returns array after player move made

      (setf turnCount (+ turnCount 1))                                   ;;;Increments the turn count by 1
      
      (setf win (checkWin gameArray player))                             ;;;Returns true if win condition met
      
      (if (eq win t)                                                     ;;;If someone wins, end the round
          (setf *endRound* t)
      )
      
      (if (and (= turnCount 9) (eq win nil))                             ;;;if game board is full, declare a tie and end the round
          (progn
            (setf tie t)
            (setf *endRound* t)
          )                       
      )

      (if (eq *endRound* nil)                                            ;;;If nobody wins during this turn, switch players
          (setf player (switchPlayer player)) 
      )
      
      (when (eq *endRound* t)  (return))                                 ;;;Exits the game loop when round ended
    );;;end of Loop
    
    (if (eq win t)                                                       ;;;after round is over with a declared winner, reset *endRound* to nil as it is used later
        (setf *endRound* nil)
    )
    
    (roundOver tie)                                                      ;;;Round over function declares that the round has ended or restarts the round in the case of a tie
    (return-from subGame player)                                         ;;;Returns the round winner to ultGame function
  );;;end of LET
);;;end of subGame function



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SWITCH PLAYER FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switchPlayer (player)
  (if (string= player "X")          ;;;If previous player is X, change to player O
      (setq player "O")
      (setq player "X")             ;;;else, set player to X
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE BOARD FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makeBoard ()
  (let ((blankArray (make-array '(3 3) 
                                :initial-contents '((" " " " " ") (" " " " " ") (" " " " " ")))))
    (return-from makeBoard blankArray)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRINT BOARD FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRINT KEY FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printKey (player)
  (format t "KEY:~%")
  (format t "1|2|3~%4|5|6~%7|8|9~%")
  (format t "(O to quit)~%")
  (format t "Player ~a Enter your move:" player)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SELECT MOVE FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun selectMove (currentArray player gameType)

  ;;; LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((selection " ")        ;;;Players selection
        (check 0))             ;;;Control variable for loop
  ;;; END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (loop
      (setf selection " ")
      (setf check 0)
      (printKey player)
      (setq selection (read))
      (format t "~%")

      (case selection
        (0 (progn (format t "Quitting Game...~%")
                  (setf check 1)
                  (setf *endRound* t)
                  (setf *quitGame* t)
                  (return-from selectMove currentArray))
        )
        (1 (cond
             ((string= (aref currentArray 0 0) " ") (progn (format t "You chose to play at square 1 (0,0)~%~%")
                                                           (setf (aref currentArray 0 0) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (2 (cond
             ((string= (aref currentArray 0 1) " ") (progn (format t "You chose to play at square 1 (0,1)~%~%")
                                                           (setf (aref currentArray 0 1) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (3 (cond
             ((string= (aref currentArray 0 2) " ") (progn (format t "You chose to play at square 1 (0,2)~%~%")
                                                           (setf (aref currentArray 0 2) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (4 (cond
             ((string= (aref currentArray 1 0) " ") (progn (format t "You chose to play at square 1 (1,0)~%~%")
                                                           (setf (aref currentArray 1 0) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (5 (cond
             ((string= (aref currentArray 1 1) " ") (progn (format t "You chose to play at square 1 (1,1)~%~%")
                                                           (setf (aref currentArray 1 1) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (6 (cond
             ((string= (aref currentArray 1 2) " ") (progn (format t "You chose to play at square 1 (1,2)~%~%")
                                                           (setf (aref currentArray 1 2) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (7 (cond
             ((string= (aref currentArray 2 0) " ") (progn (format t "You chose to play at square 1 (2,0)~%~%")
                                                           (setf (aref currentArray 2 0) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (8 (cond
             ((string= (aref currentArray 2 1) " ") (progn (format t "You chose to play at square 1 (2,1)~%~%")
                                                           (setf (aref currentArray 2 1) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (9 (cond
             ((string= (aref currentArray 2 2) " ") (progn (format t "You chose to play at square 1 (2,2)~%~%")
                                                           (setf (aref currentArray 2 2) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (otherwise (progn
                     (setq check 2)
                     (format t "INVALID ENTRY: Choose again using 1 - 9~%~%")
                     (printBoard currentArray gameType)
                   )
        )
      );;end of CASE
      
      (if (= check 0)
          (progn
            (format t "INVALID ENTRY: Space already occupied, choose again.~%~%")
            (printBoard currentArray gameType)
          )
      )
      
    (when (= check 1) (return))
    );; end of LOOP
  );; end of LET 
);; end of selectMove FUNCTION



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHECK WIN FUNCTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun checkWin (arrayToCheck player)
  (if (or (eq (checkHoriz arrayToCheck player) t) 
          (eq (checkVert arrayToCheck player) t)
          (eq (checkDiag arrayToCheck player) t))
      (progn
        (return-from checkWin t)
      )
      
  )
)

(defun checkHoriz (arrayToCheck player)
  (dotimes (i 3)
    (if (and (string= (aref arrayToCheck i 0) player) (string= (aref arrayToCheck i 1) player) (string= (aref arrayToCheck i 2) player))
        (return-from checkHoriz t)
    )
  )
)

(defun checkVert (arrayToCheck player)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ROUND OVER FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun roundOver (tie)
  (if (eq *quitGame* nil)             ;;;If *quitGame* is true, nothing happens
      (progn
        (format t "ROUND OVER!~%")
        (if (eq tie t)
            (progn
              (format t "NOBODY WINS, PLAY AGAIN~%")
              (subGame)
            )
        )
      )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULTIMATE GAME OVER FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gameOver (tie winner)
  (format t "GAME OVER!~%")
  (if (eq *quitGame* nil)            ;;;if *quitGame* is true, GAME OVER is printed and nothing else happens
      (progn
        (if (eq tie t)
            (progn
              (format t "NOBODY WINS, PLAY AGAIN~%")
              (ultGame)
            )
            (format t "CONGRATULATIONS! PLAYER ~a IS THE WINNER OF ULTIMATE TIC-TAC-TOE!~%" winner)
        )
      )
  )
)

;;;; ULTIMATE TIC-TAC-TOE GAME


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLE DECLARATIONS;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *quitGame* nil)   ;;;Bool used to indicate the game should quit immediately without declaring a winner or tie



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULTIMATE GAME FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ultGame ()
  ;;;LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((roundNum 0)                   ;;Keeps track of rounds played
        (roundWinner " ")              ;;Sets winner to nobody by default
        (ultArray (makeBoard))         ;;Initializes ult game array with all spaces
        (win nil)                      ;;Sets default win condition to nil
        (tie nil)                      ;;Sets default tie condition to nil
        (gameType "ULTIMATE"))         ;;Printed out when game board is printed
  ;;;END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;INTRODUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (format t "Welcome to ULTIMATE TIC-TAC-TOE~%~%")
    
    (format t "INSTRUCTIONS:~%")
    (format t "- The goal is to win Tic-Tac-Toe on the ULTIMATE game board.~%")
    (format t "- In order to place your symbol on the ultimate board, you must first win a round of regular Tic-Tac-Toe.~%")
    (format t "- Rounds resulting in a tie will be replayed until someone wins~%")
    (format t "- Play up to 9 rounds of Tic-Tac-Toe to declare the ultimate winner!~%")
    ;;;END-INTRODUCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (loop
      (format t "~%~%ROUND ~a~%" (+ roundNum 1))
      (setf roundWinner " ")
      (setf roundWinner (subGame))
      
      (if (eq *quitGame* nil)                                                      ;;;If player decides to quit, remainder of ultGame loop will be skipped
          (progn
            (format t "~%~%PLAYER ~a WINS ROUND ~a! CHOOSE YOUR SPOT ON THE ULTIMATE BOARD.~%" roundWinner (+ roundNum 1))
            (printBoard ultArray gameType)
            
            (if (string= roundWinner "X")                                      ;;;If its X's turn...
                (setq ultArray (selectMove ultArray roundWinner gameType))     ;;;Then, select move fcn returns array after player move made
                (setq ultArray (aiMakeMove ultArray gameType roundNum))                 ;;;else, select move fcn returns array after ai move made
            )
            
            (setf roundNum (+ roundNum 1))                                         ;;;Increments the turn count by 1
            
            (setf win (checkWin ultArray roundWinner))                             ;;;Check win returns true if win condition met
            
            (if (and (= roundNum 9) (eq win nil))                                  ;;;if game board is full, set tie to true and win to false
                (progn
                  (setf tie t)
                  (setf win nil)
                )                       
            )
          )
      )

      (when (or (eq win t) (eq tie t) (eq *quitGame* t))  (return))                    ;;;Exits the loop if tie is true, win is true, or *quitGame* is true
      );;;end of Loop
    
    (gameOver tie roundWinner)                     ;;;gameOver function declares that the game has ended or restarts the entire game in the case of a tie
  );;;end of LET
);;;end of ultGame function





;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUB GAME FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subGame ()
  
  ;;;LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;
  (let ((gameArray (makeBoard))     ;;Initializes game array to all spaces
        (player "O")                ;;Sets first player
        (win nil)                   ;;Sets default win condition to nil
        (tie nil)                   ;;Sets default tie condition to nil
        (turnCount 0)               ;;Keeps track of turns made in each round
        (gameType "GAME"))          ;;Printed out when game board is printed
  ;;;END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;

    (printBoard gameArray gameType)                               ;;;Print board
    
    (loop
      (if (eq tie t)                                                  ;;;If a tie is detected, all values reset to original
          (progn
            (setf gameArray (makeboard))
            (setf player "O")
            (setf tie nil)
            (setf win nil)
            (setf turnCount 0)
            (printBoard gameArray gameType)
          ) 
      )

      (setf player (switchPlayer player))

      (if (string= player "X")
          (setq gameArray (selectMove gameArray player gameType))     ;;;select move fcn returns array after player move made
          (setq gameArray (aiMakeMove gameArray gameType turnCount))  ;;;select move fcn returns array after ai move made
      )

      (setf turnCount (+ turnCount 1))                            ;;;Increments the turn count by 1
      
      (setf win (checkWin gameArray player))                      ;;;Returns true if win condition met
      
      (if (and (= turnCount 9) (eq win nil))                      ;;;if game board is full and there is no winner, declare tie is true and win is false
          (progn
            (setf tie t)
            (setf win nil)
            (format t "NOBODY WINS, PLAY AGAIN~%")
          )
      )
      
      (when (or (eq win t) (eq *quitGame* t))   (return))         ;;;Exits the game loop when there is a winner or when quit game is true
    );;;end of Loop

    (if (eq *quitGame* nil)                                       ;;;If quit game is selected, skip the printing of "ROUND OVER".
        (format t "ROUND OVER!~%")
    )
    
    (return-from subGame player)                                  ;;;Returns the round winner to ultGame function
    
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
  (return-from switchPlayer player)
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
  (setf *quitGame* nil)

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
             ((string= (aref currentArray 0 1) " ") (progn (format t "You chose to play at square 2 (0,1)~%~%")
                                                           (setf (aref currentArray 0 1) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (3 (cond
             ((string= (aref currentArray 0 2) " ") (progn (format t "You chose to play at square 3 (0,2)~%~%")
                                                           (setf (aref currentArray 0 2) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (4 (cond
             ((string= (aref currentArray 1 0) " ") (progn (format t "You chose to play at square 4 (1,0)~%~%")
                                                           (setf (aref currentArray 1 0) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (5 (cond
             ((string= (aref currentArray 1 1) " ") (progn (format t "You chose to play at square 5 (1,1)~%~%")
                                                           (setf (aref currentArray 1 1) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (6 (cond
             ((string= (aref currentArray 1 2) " ") (progn (format t "You chose to play at square 6 (1,2)~%~%")
                                                           (setf (aref currentArray 1 2) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (7 (cond
             ((string= (aref currentArray 2 0) " ") (progn (format t "You chose to play at square 7 (2,0)~%~%")
                                                           (setf (aref currentArray 2 0) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (8 (cond
             ((string= (aref currentArray 2 1) " ") (progn (format t "You chose to play at square 8 (2,1)~%~%")
                                                           (setf (aref currentArray 2 1) player)
                                                           (setf check 1)
                                                           (printBoard currentArray gameType)
                                                           (return-from selectMove currentArray))))
        )
        (9 (cond
             ((string= (aref currentArray 2 2) " ") (progn (format t "You chose to play at square 9 (2,2)~%~%")
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULTIMATE GAME OVER FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gameOver (tie winner)
  (format t "GAME OVER!~%")
  (if (eq *quitGame* nil)            ;;;if *quitGame* is true, GAME OVER is printed and nothing else happens
      (progn
        (if (eq tie t)
            (progn
              (format t "NOBODY WINS, RESTARTING GAME.~%")
              (ultGame)
            )
            (format t "CONGRATULATIONS! PLAYER ~a IS THE WINNER OF ULTIMATE TIC-TAC-TOE!~%" winner)
        )
      )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AI MAKE MOVE FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aiMakeMove (currentArray gameType turnCount)
  ;;; LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((bestScore -10000) 
        (score 0)
        (player "X")
        (xCoord 0)
        (yCoord 0))
  ;;; END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (dotimes (i 3)
      (dotimes (j 3)
        (if (string= (aref currentArray i j) " ") ;;;If the current space is empty, it can be considered as a possible move
            (progn
              (setf (aref currentArray i j) "O")                       ;;;sets the first available position to O to check if it is the best move
              (setf score (minMax currentArray player turnCount))      ;;;recursive function which finds optimal win condition
              (setf (aref currentArray i j) " ")                       ;;;resets  position after checking (alternative to copying the array for testing)

              (if (> score bestScore)
                  (progn
                    (setf bestScore score)
                    (setf xCoord i)
                    (setf yCoord j)
                  )
              )
              
            )    
        )
      )
    )
    (setf (aref currentArray xCoord yCoord) "O")
    (format t "AI choosing move...~%")
    (sleep 1)
    (format t "Player O chose to play at square (~a,~a)~%" xCoord yCoord)
    (printBoard currentArray gameType)          ;;;print the board before exiting the function
    (return-from aiMakeMove currentArray)       ;;;return the array with newly added ai move to game function
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIN-MAX FUNCTIONS FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minMax (currentArray player turnCount)
  ;;; LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let ((xScore -10)
        (oScore 10)
        (tieScore 0)
        (win nil)
        (bestScore 0)
        (score 0)) 
  ;;; END: LOCAL VAR DECLARATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (setf turnCount (+ turnCount 1))
    (setf win (checkWin currentArray player))


    ;;Depending on win (x or o) or tie, return the corresponding score
    (if (and (eq win t) (string= player "X"))
        (progn
          (return-from minMax xScore)
        )     
    )
    (if (and (eq win t) (string= player "O"))
        (progn
          (return-from minMax oScore)
        )
    )
    (if (and (eq win nil) (= turnCount 9)) 
        (progn
          (return-from minMax tieScore)
        )
    )
    
    (if (string= player "O")    ;;Player O is looking to get the MAX score
        (progn
          (setf bestScore -10000)
          (dotimes (i 3)
            (dotimes (j 3)
              (if (string= (aref currentArray i j) " ")
                  (progn
                    (setf (aref currentArray i j) "O")
                    (setf score (minMax currentArray "X" turnCount))
                    (setf (aref currentArray i j) " ")
                    (setf bestScore (max score bestscore))
                  )    
              )
            )
          )
          (return-from minMax bestScore)
        )     
    )
    
    (if (string= player "X")    ;;;Player X is looking to get the MIN score
        (progn
          (setf bestScore 10000)
          (dotimes (i 3)
            (dotimes (j 3)
              (if (string= (aref currentArray i j) " ")
                  (progn
                    (setf (aref currentArray i j) "X")
                    (setf score (minMax currentArray "O" turnCount))
                    (setf (aref currentArray i j) " ")
                    (setf bestScore (min score bestscore))
                  )    
              )
            )
          )
          (return-from minMax bestScore)
        )   
    ) 
  )
)

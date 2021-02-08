;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; PROJECT NAME:          UltimateTicTacToe.lisp (Group 10)
;;;;
;;;; GROUP MEMBERS:         Xavier Julius - xjj1002
;;;;                        Logan Racer   - lsr1006
;;;;                        (with assistance from Tanuj Rane - txr1029)
;;;;
;;;; STATUS:                Correctly - Works Completely
;;;;
;;;; PLAGIARISM STATEMENT:  This program was done entirely by us and no part of this program was plagiarized,
;;;;                        intentionally or unintentionally, from anybody.  We would be held accountable and
;;;;                        penalized if any part of this program was plagiarized.
;;;;
;;;; DESCRIPTION:           - A game of Ultimate Tic-Tac_toe to be played in the LISP console.
;;;;                        - Run and play from the *slime-repl sbcl* buffer by entering (ultGame).
;;;;
;;;; RESOURCES:             All sources referenced are included in the Resource Document. Code derived from
;;;;                        references will be indicated in a comment with [REFERENCE NUMBER].
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GLOBAL VARIABLES:
(defvar *quitGame* nil)                ; Boolean used to indicate the game should quit immediately

;;; GLOBAL CONSTANTS:
;;; Note: (defconstant should be used instead, but it was giving compiler errors that we could not fix)
(defvar +PLAYER_1+ "X")                ; Symbol used by Player 1
(defvar +PLAYER_2+ "O")                ; Symbol used by Player 2
(defvar +GAME_SIZE+ 3)                 ; Width/Height of the 2D game board arrays
(defvar +MAX_TURNS+ 9)                 ; Total number of spaces on the game board arrays

;;;; ultGame Function: No parameters. No returns. Call this function to start the game. Controls the flow of the game.
(defun ultGame ()
  ;;;LOCAL VARIABLES:
  (let ((roundNum 0)                   ; Keeps track of rounds played.
        (roundWinner " ")              ; Sets winner to blank by default.
        (ultArray (makeBoard))         ; Initializes array ultGame with all spaces.
        (win nil)                      ; Sets default win condition to false.
        (tie nil)                      ; Sets default tie condition to false.
        (gameType "ULTIMATE"))         ; Printed out in heading when printBoard function called.

    ;;;INTRODUCTION:
    (format t "Welcome to ULTIMATE TIC-TAC-TOE~%~%")
    (format t "INSTRUCTIONS:~%")
    (format t "- The goal is to win Tic-Tac-Toe on the ULTIMATE game board.~%")
    (format t "- In order to place your symbol on the ultimate board, you must first win a round of regular Tic-Tac-Toe.~%")
    (format t "- Rounds resulting in a tie will be replayed until someone wins~%")
    (format t "- Play up to 9 rounds of Tic-Tac-Toe to declare the ultimate winner!~%")

    (loop
      (format t "~%~%ROUND ~a~%" (+ roundNum 1))
      (setf roundWinner " ")                           ; Ensures roundWinner is blank
      (setf roundWinner (subGame))                     ; subGame returns the winner of a round
      
      (if (eq *quitGame* nil)      ; If player decides to quit, remainder of ultGame loop will be skipped
          (progn
            (format t "~%~%PLAYER ~a WINS ROUND ~a! CHOOSE YOUR SPOT ON THE ULTIMATE BOARD.~%" roundWinner (+ roundNum 1))
            (printBoard ultArray gameType)
            
            (if (string= roundWinner +PLAYER_1+)                               ; If its Player 1's turn...
                (setq ultArray (selectMove ultArray roundWinner gameType))     ; Then, selectMove returns ultArray after player move made
                (setq ultArray (aiMakeMove ultArray gameType roundNum))        ; else, aiMakeMove returns ultArray after ai move made
            )
            
            (setf roundNum (+ roundNum 1))                                     ; Increments the turn count by 1
            (setf win (checkWin ultArray roundWinner))                         ; checkWin returns true if a  win condition is met

            ;; if game board is full, set tie to true and win to false
            (if (and (= roundNum +MAX_TURNS+) (eq win nil))                              
                (progn
                  (setf tie t)
                  (setf win nil)
                )
            )
          )
      )
      
      (when (or (eq win t) (eq tie t) (eq *quitGame* t)) (return))    ; Exits the loop if tie is true, win is true, or *quitGame* is true
    )
    (gameOver tie roundWinner)       ; gameOver function declares that the game has ended or restarts the entire game in the case of a tie
  )
)

;;;; subGame function: No parameters. Returns PLAYER representing the round winner. Controls the flow of each round.
(defun subGame ()
  
  ;; LOCAL VARIABLES:
  (let ((gameArray (makeBoard))     ; Initializes gameArray with all spaces
        (player +PLAYER_2+)         ; Sets first player
        (win nil)                   ; Sets default win condition to false
        (tie nil)                   ; Sets default tie condition to false
        (turnCount 0)               ; Keeps track of turns made in each round
        (gameType "GAME"))          ; Printed out in heading when printBoard function called.

    (printBoard gameArray gameType)
    
    (loop
      ;; If tie is true, all values reset to original to restart the round
      (if (eq tie t)
          (progn
            (setf gameArray (makeboard))
            (setf player +PLAYER_2+)
            (setf tie nil)
            (setf win nil)
            (setf turnCount 0)
            (printBoard gameArray gameType)
          ) 
      )

      (setf player (switchPlayer player))                              ; Switches the player

      (if (string= player +PLAYER_1+)                                  ; If its Player 1's turn...
          (setq gameArray (selectMove gameArray player gameType))      ; Then, selectMove returns gameArray after player move made
          (setq gameArray (aiMakeMove gameArray gameType turnCount))   ; else, aiMakeMove returns gameArray after ai move made
      )

      (setf turnCount (+ turnCount 1))                   ; Increments the turn count by 1
      (setf win (checkWin gameArray player))             ; Returns true if a win condition is met

      ;; if game board is full and there is no winner, set tie to true and win to false
      (if (and (= turnCount +MAX_TURNS+) (eq win nil))
          (progn
            (setf tie t)
            (setf win nil)
            (format t "NOBODY WINS, PLAY AGAIN~%")
          )
      )
      
      (when (or (eq win t) (eq *quitGame* t))   (return))    ; Exits the game loop when there is a winner or when quit game is true
    )

    (if (eq *quitGame* nil)                                  ; If quit game is selected, skip the printing of "ROUND OVER".
        (format t "ROUND OVER!~%")
    )
    
    (return-from subGame player)                             ; Returns PLAYER (winner of the round) to ultGame function
    
  )
)

;;;; switchPlayer function: Takes one parameter, PLAYER. Returns PLAYER. Switches between players.
(defun switchPlayer (player)
  (if (string= player +PLAYER_1+)          ; If previous player is Player 1
      (setq player +PLAYER_2+)             ; Then, set player to Player 2
      (setq player +PLAYER_1+)             ; Else, set player to Player 1
  )
  (return-from switchPlayer player)
)

;;;; makeBoard function: No parameters. Returns blankArray. Creates a 2D array of blank spaces
(defun makeBoard ()
  (let ((blankArray (make-array '(3 3)
                                :initial-contents '((" " " " " ") (" " " " " ") (" " " " " ")))))
    (return-from makeBoard blankArray)
  )
)

;;;; printBoard function: 2 Parameters, arrayToPrint and gameType. No returns. Prints out an array formatted as a game board.
(defun printBoard (arrayToPrint gameType)
  (format t "~a BOARD:~%" gameType)
  (dotimes (i +GAME_SIZE+)
    (format t "|")
    (format t (aref arrayToPrint i 0))
    (format t "|")
    (format t (aref arrayToPrint i 1))
    (format t "|")
    (format t (aref arrayToPrint i 2))
    (format t "|")
    (terpri)      ; New line
    )
  (format t "~%")
)

;;;; printKey function: 1 parameter, PLAYER. No return. Prints out the key for the move entry prompt.
(defun printKey (player)
  (format t "KEY:~%")
  (format t "1|2|3~%4|5|6~%7|8|9~%")
  (format t "(0 to quit)~%")
  (format t "Player ~a Enter your move:" player)
)

;;;; selectMove function: 3 parameters, currentArray, player, and gameType. Returns currentArray. Accepts player input and sets the
;;;;                                                                                              corresponding symbol in the array.
(defun selectMove (currentArray player gameType)
  (setf *quitGame* nil)

  ;; LOCAL VARIABLES:
  (let ((selection " ")        ; Players selection initialized to space
        (check 0))             ; Control variable for loop. Loop only ends if check = 1
    
    (loop
      (setf selection " ")     ; Resets selection to space every time the loop is run
      (setf check 0)           ; Resets check to 0 every time the loop is run
      (printKey player)        ; Prints the key prompt every time the loop is run
      (setq selection (read))  ; reads in player selection
      (format t "~%")          ; Newline

      (case selection
        ;; If quit selected, check set to 1 so loop exits, *quitGame* set to true so game will quit
        (0 (progn (format t "Quitting Game...~%")
                  (setf check 1)
                  (setf *quitGame* t)
                  (return-from selectMove currentArray))
        )
        ;; For cases 1 - 9, if space is blank, player can play at that space
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
        ;; If a character other than 0 - 9 is entered, set check to 2 so loop does not exit
        (otherwise (progn
                     (setq check 2)
                     (format t "INVALID ENTRY: Choose again using 0 - 9~%~%")
                     (printBoard currentArray gameType)
                   )
        )
      )
      ;; If check = 0, the space is already occupied and the loop will not exit
      (if (= check 0)
          (progn
            (format t "INVALID ENTRY: Space already occupied, choose again.~%~%")
            (printBoard currentArray gameType)
          )
      )
      
    (when (= check 1) (return))       ; When check is 1, exit the loop. Valid selection was made
    )
  )
)

;;;; checkWin function: 2 Parameters, arrayToCheck and player. Returns true. If any of the win conditions are met, return true.
(defun checkWin (arrayToCheck player)
  (if (or (eq (checkHoriz arrayToCheck player) t) 
          (eq (checkVert arrayToCheck player) t)
          (eq (checkDiag arrayToCheck player) t))
      (return-from checkWin t)
  )
)

;;;; checkHoroz function: 2 Parameters, arrayToCheck and player. Returns true. If horizontal win condition met, return true.
(defun checkHoriz (arrayToCheck player)
  (dotimes (i +GAME_SIZE+)
    (if (and (string= (aref arrayToCheck i 0) player) (string= (aref arrayToCheck i 1) player) (string= (aref arrayToCheck i 2) player))
        (return-from checkHoriz t)
    )
  )
)

;;;; checkVert function: 2 Parameters, arrayToCheck and player. Returns true. If vertical win condition met, return true.
(defun checkVert (arrayToCheck player)
  (dotimes (i +GAME_SIZE+)
    (if (and (string= (aref arrayToCheck 0 i) player) (string= (aref arrayToCheck 1 i) player) (string= (aref arrayToCheck 2 i) player))
        (return-from checkVert t)
    )
  ) 
)

;;;; checkDiag function: 2 Parameters, arrayToCheck and player. Returns true. If one diagonal win condition met, return true.
(defun checkDiag (arrayToCheck player)
  (if (and (string= (aref arrayToCheck 0 0) player) (string= (aref arrayToCheck 1 1) player) (string= (aref arrayToCheck 2 2) player))
      (return-from checkDiag t)
  )
  
  (if (and (string= (aref arrayToCheck 2 0) player) (string= (aref arrayToCheck 1 1) player) (string= (aref arrayToCheck 0 2) player))
      (return-from checkDiag t)
  )
)

;;;; gameOver function: 2 parameters, tie and winner. No return. Outputs text at the end of the game or restarts the game.
(defun gameOver (tie winner)
  (format t "GAME OVER!~%")
  ;; If *quitGame* is true, nothing else is printed.
  (if (eq *quitGame* nil)
      (progn
        ;; if tie is true, restart the game
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

;;;; aiMakeMove function: 3 parameters, currentArray, gametype, and turnCount. Returns currentArray. One of two functions used to determine
;;;;                                                                                                 the AI's move.
;;;; Note: This function was inspired by source [1] and [2]
(defun aiMakeMove (currentArray gameType turnCount)
  ;; LOCAL VARIABLES
  (let ((bestScore -10000)         ; Keeps track of the bestScore or each location. Initially set to worst-case scenario.
        (score 0)                  ; Score to be returned from minMax
        (player +PLAYER_1+)        ; Sets initial player to Player 1 for use in minMax function.
        (xCoord 0)                 ; Coordinates used to record position of optimal move.
        (yCoord 0))
    
    ;; Nested loops iterate through all empty spaces on the board
    (dotimes (i +GAME_SIZE+)
      (dotimes (j +GAME_SIZE+)
        (if (string= (aref currentArray i j) " ")                  ; If the current space is empty, it can be considered as a possible move.
            (progn
              (setf (aref currentArray i j) +PLAYER_2+)            ; Sets the next blank space to Player 2 for testing.
              (setf score (minMax currentArray player turnCount))  ; Returns a score based on all possible terminal states of the board.
              (setf (aref currentArray i j) " ")                   ; After getting a score for each position, it is reset to blank.

              ;; If a new bestScore is found...
              (if (> score bestScore)
                  (progn
                    (setf bestScore score)     ; bestScore is updated
                    (setf xCoord i)            ; Corresponding coordinates are recorded
                    (setf yCoord j)
                  )
              )
              
            )
        )
      )
    )

    (setf (aref currentArray xCoord yCoord) +PLAYER_2+)    ; Sets optimal position to Player 2 for the AI's turn.
    (format t "AI choosing move...~%")
    (sleep 1)                                       ; Pauses execution for one unit of time
    (format t "Player ~a chose to play at square (~a,~a)~%" +PLAYER_2+ xCoord yCoord)
    (printBoard currentArray gameType)
    (return-from aiMakeMove currentArray)           ; Return the updated array
  )
)

;;;; minMax function: 3 parameters, currentArray, player, and turnCount. Returns score or bestScore. Recursive function used to determine
;;;;                                                                                                 the optimal move for the AI.
;;;; Note: This function was inspired by source [1] and [2]
(defun minMax (currentArray player turnCount)
  ;; LOCAL VARIABLES
  (let ((p1Score -10)       ; Low score corresponding to terminal board states where Player 1 is the winner.
        (p2Score 10)        ; High score corresponding to terminal board states where Player 2 is the winner.
        (tieScore 0)       ; Middle score corresponding to terminal board states where there is a tie.
        (win nil)          ; Used to record if a specific iteration has a winner.
        (bestScore 0)      ; Keeps track of the bestScore of all terminal board states of a given move.
        (score 0))         ; Keeps track of the score for every terminal board state.
    
    (setf turnCount (+ turnCount 1))
    (setf win (checkWin currentArray player))

    ;; Depending on win (Player 1 or Player 2) or tie, return the corresponding score
    (if (and (eq win t) (string= player +PLAYER_1+))
        (progn
          (return-from minMax p1Score)
        )     
    )
    (if (and (eq win t) (string= player +PLAYER_2+))
        (progn
          (return-from minMax p2Score)
        )
    )
    (if (and (eq win nil) (= turnCount +MAX_TURNS+)) 
        (progn
          (return-from minMax tieScore)
        )
    )

    ;;; If the above conditions are not true, a terminal board state has not yet been reached
    
    ;; Player 2 is looking for the maximum score to win
    (if (string= player +PLAYER_2+)
        (progn
          (setf bestScore -10000)       ; bestScore set to the most undesirable score for Player 2
          (dotimes (i +GAME_SIZE+)                ; iterate through 
            (dotimes (j +GAME_SIZE+)
              (if (string= (aref currentArray i j) " ")                     ; If the current space is empty, it can be considered as a possible move.
                  (progn
                    (setf (aref currentArray i j) +PLAYER_2+)               ; Sets the next blank space to Player 2 for testing.
                    (setf score (minMax currentArray +PLAYER_1+ turnCount)) ; Recursive function call, this time using the other player
                    (setf (aref currentArray i j) " ")                      ; After getting a score, it is reset to blank.
                    (setf bestScore (max score bestscore))                  ; bestScore is set to the max value of all scores
                  )    
              )
            )
          )
          (return-from minMax bestScore)  ; bestScore returned
        )     
    )

    ;; Player 1 is looking for the minimum score to win
    (if (string= player +PLAYER_1+)
        (progn
          (setf bestScore 10000)           ; bestScore set to the most undesirable score for Player 1
          (dotimes (i +GAME_SIZE+)
            (dotimes (j +GAME_SIZE+)
              (if (string= (aref currentArray i j) " ")                     ; If the current space is empty, it can be considered as a possible move.
                  (progn
                    (setf (aref currentArray i j) +PLAYER_1+)               ; Sets the next blank space to player 1 for testing.
                    (setf score (minMax currentArray +PLAYER_2+ turnCount)) ; Recursive function call, this time using the other player
                    (setf (aref currentArray i j) " ")                      ; After getting a score, it is reset to blank.
                    (setf bestScore (min score bestscore))                  ; bestScore is set to the min value of all scores
                  )    
              )
            )
          )
          (return-from minMax bestScore)   ;bestScore returned
        )   
    ) 
  )
)

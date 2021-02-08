;;;; ULTIMATE TIC-TAC-TOE GAME


;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *quit* nil)   ;;;bool used to indicate the game should end
(defvar *tempMove* )
(defvar oppm)        ;;players move
(setf aiArray (make-array '(3 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULTIMATE GAME FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ultGame ()
  ;;(setf *quit* nil)
  (format t "Welcome to ULTIMATE TIC-TAC-TOE~%")
  (format t "Play up to 9 rounds of tic tac toe to declare the ultimate winner!~%")
  (format t "Winner of each round gets to play on the ultimate board.~%")

  (let ((roundNum 1)
        (roundWinner " ")
        (ultArray (makeBoard))
        (win nil)
        (tie nil)
        (gameType "ULTIMATE"))

    (loop
      (format t "ROUND ~a~%" roundNum)
      (setf roundWinner (subGame))
      (format t "PLAYER ~a WINS ROUND ~a! CHOOSE YOUR SPOT ON THE ULTIMATE BOARD.~%" roundWinner roundNum)
      ;;(printBoard ultArray gameType)
      (if (string= player "X")                                                        ;;;if players turn
          (setq ultArray (selectMove ultArray player gameType))                     ;;;select move fcn returns player selection in form of a list
          (setq ultArray (bestm ultArray))                                          ;;; else, return from ai stuff
      )
      
      (printBoard ultArray gametype)
      (setf roundNum (+ roundNum 1))                                   ;;;Increments the turn count by 1
      
      (setf win (checkWin ultArray roundWinner))                             ;;;Check win returns true if win condition met
      (if (eq win t)
          (setf *quit* t)
          )
      
      (if (and (= roundNum 9) (eq win nil))                                  ;;;if game board is full, declare a tie and set quit to true
          (progn
            (setf tie t)
            (setf *quit* t)
          )                       
      )

      (when (eq *quit* t)  (return))
      )
    
    (gameOver tie)
    (format t "CONGRATULATIONS! PLAYER ~a IS THE WINNER OF ULTIMATE TIC-TAC-TOE!~%" roundWinner)
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUB GAME FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subGame ()
  (let ((gameArray (makeBoard))
        (player "X")
        (win nil)
        (tie nil)
        (turnCount 0)
        (gameType "GAME"))
    
    (setf *quit* nil)
    ;;(printBoard gameArray gameType)                                      ;;;Print board
    (loop
      
      (if (string= player "X")                                                        ;;;if players turn
          (setq gameArray (selectMove gameArray player gameType))                     ;;;select move fcn returns player selection in form of a list
          (setq gameArray (bestm gameArray))                                          ;;; else, return from ai stuff
      )
      
      
      (printBoard gameArray gametype)                                      ;;;print board

      (setf turnCount (+ turnCount 1))                                   ;;;Increments the turn count by 1
      
      (setf win (checkWin gameArray player))                             ;;;Check win returns true if win condition met
      (if (eq win t)
          (setf *quit* t)
      )
     
      (if (and (= turnCount 9) (eq win nil))                                                ;;;if game board is full, declare a tie and set quit to true
          (progn
            (setf tie t)
            (setf *quit* t)
          )                       
      )

      (if (eq *quit* nil)
          (setf player (switchPlayer player)) 
      )
      
    (when (eq *quit* t)  (return))                                         ;;;Exits the game loop when win condition met
    )
    (if (eq win t)
        (setf *quit* nil)
    )
    (roundOver tie)
    (return-from subGame player)
  )
)



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
  (let ((selection " ") (check 0))

    (loop

      (setf selection " ")
      (setf check 0)
      (printBoard currentArray gameType)
      (printKey player)
      (setq selection (read))
      (format t "~%")

      (case selection
        (0 (progn (format t "Quitting Game~%")
                  (setf check 1)
                  (setf *quit* t)
                  (return-from selectMove currentArray))
         )
        (1 (cond
             ((string= (aref currentArray 0 0) " ") (progn (format t "You chose to play at square 1 (0,0)~%")
                                                           (setf (aref currentArray 0 0) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (2 (cond
             ((string= (aref currentArray 0 1) " ") (progn (format t "You chose to play at square 1 (0,1)~%")
                                                           (setf (aref currentArray 0 1) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (3 (cond
             ((string= (aref currentArray 0 2) " ") (progn (format t "You chose to play at square 1 (0,2)~%")
                                                           (setf (aref currentArray 0 2) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (4 (cond
             ((string= (aref currentArray 1 0) " ") (progn (format t "You chose to play at square 1 (1,0)~%")
                                                           (setf (aref currentArray 1 0) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (5 (cond
             ((string= (aref currentArray 1 1) " ") (progn (format t "You chose to play at square 1 (1,1)~%")
                                                           (setf (aref currentArray 1 1) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (6 (cond
             ((string= (aref currentArray 1 2) " ") (progn (format t "You chose to play at square 1 (1,2)~%")
                                                           (setf (aref currentArray 1 2) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (7 (cond
             ((string= (aref currentArray 2 0) " ") (progn (format t "You chose to play at square 1 (2,0)~%")
                                                           (setf (aref currentArray 2 0) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (8 (cond
             ((string= (aref currentArray 2 1) " ") (progn (format t "You chose to play at square 1 (2,1)~%")
                                                           (setf (aref currentArray 2 1) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (9 (cond
             ((string= (aref currentArray 2 2) " ") (progn (format t "You chose to play at square 1 (2,2)~%")
                                                           (setf (aref currentArray 2 2) player)
                                                           (setf check 1)
                                                           (return-from selectMove currentArray))))
         )
        (otherwise (progn
                     (format t "ERRROR: Invalid Selection, please choose again~%")
                     )
         )
        )
      (if (= check 0)
          (write "ERRROR: Invalid Selection, please choose again"))
      
      (when (= check 1) (return))
      )
    ) 
  )



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
  (format t "ROUND OVER!~%")
  (if (eq tie t)
      (progn
        (format t "NOBODY WINS, PLAY AGAIN~%")
        (subGame)
      )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULTIMATE GAME OVER FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gameOver (tie)
  (format t "GAME OVER!~%")
  (if (eq tie t)
      (progn
        (format t "NOBODY WINS, PLAY AGAIN~%")
        (ultGame)
      )
  )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AI STUFF;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defun maketembb (gameArray)
  (setf (aref aiArray 0 0) (aref gameArray 0 0) )
  (setf (aref aiArray 0 1) (aref gameArray 0 1) )
  (setf (aref aiArray 0 2) (aref gameArray 0 2) )
  (setf (aref aiArray 1 0) (aref gameArray 1 0) )
  (setf (aref aiArray 1 1) (aref gameArray 1 1) )
  (setf (aref aiArray 1 2) (aref gameArray 1 2) )
  (setf (aref aiArray 2 0) (aref gameArray 2 0) )
  (setf (aref aiArray 2 1) (aref gameArray 2 1) )
  (setf (aref aiArray 2 2) (aref gameArray 2 2) )
  ;; (print aiarray)
  )

(defun bestm (currentArray)
  
  (defvar i)
  (defvar j)
  (setq i 0)
  (setq j 0)
  (defvar k)
  (defvar l)
  (defvar move)
  (defvar player "X")
  (setq move 100)
  (setq k 100)
  (setq l 100)

  (format t "K before:~a~%" k)
  (format t "L before:~a~%" l)
  
  (dotimes (i 3)
    
    (dotimes (j 3)
      (maketembb currentArray)
      (setq *tempMove* 100)
      (setq oppm 0)
      ;;(print aiarray)
      (cond    
        ((string= (aref aiArray i j) " ") (progn (setf (aref aiArray i j) "O")
                                                 (format t "i: ~a j: ~a~%" i j)
                                                 (format t "Array at i j: ~a~%" (aref aiArray i j))))
        
        )
      (print aiarray)
      
      (checkcp aiarray player)
      
      (cond ((= oppm 200)(progn (setq *tempMove* 1) )))
      
      (checkrp aiArray player)

      (cond ((= oppm 200)(progn (setq *tempMove* 1) )))
      
      (checkdp aiArray player)
 
      (cond ((= oppm 200)(progn (setq *tempMove* 1) )))
      
   
      (cond ((>= move *tempMove*)  (progn (setq k i) (setq l j) (setq move *tempMove*))))

      (format t "~%")
      (format t "K:~a~%" k)
      (format t "L:~a~%" l)
      (format t "move:~a~%" move)
      (format t "Tempmove:~a~%" *tempMove*)
      
      )

    )
  (setf (aref currentArray k l) "O")
 
  (return-from bestm currentArray)
)





(defun checkcp (currentArray opp)
  (defvar x 0)
  (setq x 0)
  (defvar y 0)
  
  

  (dotimes (x 3)
    (setq y 0)

    (cond    
      ((string= (aref currentArray 0 x) " ") (progn (setq y (+ y 1))))
      
      )
    
    (cond
      ((string= (aref currentArray 1 x) " ") (progn (setq y (+ y 1))))
      )


    (cond
      ((string= (aref currentArray 2 x) " ") (progn (setq y (+ y 1))))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (cond    
      ((string= (aref currentArray 0 x) "X") (progn (setq y (+ y 100))))
      
      )
    
    (cond
      ((string= (aref currentArray 1 x) "X") (progn (setq y (+ y 100))))
      )


    (cond
      ((string= (aref currentArray 2 x) "X") (progn (setq y (+ y 100))))
      )

    (cond ((< y *tempMove*)(progn (setq *tempMove* y))))
    (cond ((= y 200)(progn (setq oppm y))))
    (cond ((= y 0)(progn (setq oppm 0)(setq *tempMove* 0))))

    )



  )



(defun checkrp (currentArray opp)
  (defvar x 0)
  (setq x 0)
  
  (defvar y 0)
  (setq y 0)
  

  (dotimes (x 3)
    (setq y 0)

    (cond    
      ((string= (aref currentArray x 0) " ") (progn (setq y (+ y 1))))
      
      )
    
    (cond
      ((string= (aref currentArray x 1) " ") (progn (setq y (+ y 1))))
      )


    (cond
      ((string= (aref currentArray x 2) " ") (progn (setq y (+ y 1))))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (cond    
      ((string= (aref currentArray x 0) "X") (progn (setq y (+ y 100))))
      
      )
    
    (cond
      ((string= (aref currentArray x 1) "X") (progn (setq y (+ y 100))))
      )


    (cond
      ((string= (aref currentArray x 2) "X") (progn (setq y (+ y 100))))
      )
    
    (print y)

    (cond ((< y *tempMove*)(progn   (setq *tempMove* y))))
    (cond ((= y 200)(progn   (setq oppm y))))
    (cond ((= y 0)(progn (setq oppm 0)(setq *tempMove* 0))))

    )
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun checkdp (currentArray opp)
  (defvar x 0)
  (setq x 0)
  (defvar y 0)
  (setq y 0)
  

  (dotimes (x 3)
    (setq y 0)

    (cond    
      ((string= (aref currentArray 0 (+ 0 x))" ") (progn (setq y (+ y 1))))
      
      )
    
    (cond
      ((string= (aref currentArray 1 1) " ") (progn (setq y (+ y 1))))
      )


    (cond
      ((string= (aref currentArray 2 (- 2 x)) " ") (progn (setq y (+ y 1))))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (cond    
      ((string= (aref currentArray 0 (+ 0 x)) "X") (progn (setq y (+ y 100))))
      
      )
    
    (cond
      ((string= (aref currentArray 1 1) "X") (progn (setq y (+ y 100))))
      )


    (cond
      ((string= (aref currentArray 2 (- 2 x)) "X") (progn (setq y (+ y 100))))
      )

    
    (cond ((< y *tempMove*)(progn   (setq *tempMove* y))))
    (cond ((= y 200)(progn   (setq oppm y))))
    (cond ((= y 0)(progn (setq oppm 0)(setq *tempMove* 0))))
    

    )


  )

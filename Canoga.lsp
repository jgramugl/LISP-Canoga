;************************************************************
;* Name:  Joe Gramuglia                                     *
;* Project:  Canoga                                         *
;* Class:  OPL                                              *
;* Date:  10/16/2015                                        *
;************************************************************

;********************************************************************* 
;Function Name: cover 
;Purpose: To cover a square on the board
;Parameters: 
;            square, an integer. The square to cover
;            board, a list of integers. The board to cover 
;Return Value: The board with the given square covered.
;Local Variables: None
;Algorithm: 
;            1) If the board is empty, return an empty list
;            2) If the first element in board is the square, return the rest of the board with a 0 in front
;            3) Else recurse into this function again using the rest of the board, returns the list of the recursed function with the first of the board added in front
;Assistance Received: none 
;*********************************************************************
(defun cover(square board)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((= square (first board))
          (cons 0 (rest board))
        )
        (t
          (cons (first board) (cover square (rest board)))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: uncover 
;Purpose: To uncover a square on the board
;Parameters: 
;            square, an integer. The square to uncover
;            board, a list of integers. The board to uncover 
;            count, an integers. Represents the number of the square that is covered [Covered squares are represented with a 0]
;Return Value: The board with the given square uncovered.
;Local Variables: None
;Algorithm: 
;            1) If the board is empty, return an empty list
;            2) If the first element in board is the square, return the rest of the board with the count added in front
;            3) Else recurse into this function again using the rest of the board and an incremented count, returns the list of the recursed function with the first of the board added in front
;Assistance Received: none 
;*********************************************************************
(defun uncover(square board count)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((= square count)
          (cons square (rest board))
        )
        (t
          (cons (first board) (uncover square (rest board) (+ count 1)))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: coverCombo
;Purpose: To cover a series of squares on the board
;Parameters: 
;            combination, a list of integers. A list of numbers to cover
;            board, a list of integers. The board to cover
;Return Value: The board with the given series of squares covered.
;Local Variables: None
;Algorithm: 
;            1) If the combination is empty, return the board [Nothing to change]
;            2) Else recurse into this function again using the rest of the combination, return a list from the recursed function with the first in the combination covered
;Assistance Received: none 
;*********************************************************************
(defun coverCombo(combination board)
  (cond
    ((null combination)
      board
    )
    (t
      (cover (first combination) (coverCombo (rest combination) board))
    )
  )
)

;********************************************************************* 
;Function Name: uncoverCombo
;Purpose: To uncover a series of squares on the board
;Parameters: 
;            combination, a list of integers. A list of numbers to uncover
;            board, a list of integers. The board to uncover
;Return Value: The board with the given series of squares uncovered.
;Local Variables: None
;Algorithm: 
;            1) If the combination is empty, return the board [Nothing to change]
;            2) Else recurse into this function again using the rest of the combination, return a list from the recursed function with the first in the combination uncovered
;Assistance Received: none 
;*********************************************************************
(defun uncoverCombo(combination board)
  (cond
    ((null combination)
      board
    )
    (t
      (uncover (first combination) (uncoverCombo (rest combination) board) 1)
    )
  )
)

;********************************************************************* 
;Function Name: printSquares
;Purpose: To print a series of squares to the screen
;Parameters: 
;            board, a list of integers. The board to be printed to the screen
;Return Value: True
;Local Variables: None
;Algorithm: 
;            1) If the board is empty, return [Nothing to change]
;            2) If the first of the board is a 0 [covered], print *
;            3) Else print the first of the board
;            4) Recurse into this function until it's empty
;Assistance Received: none 
;*********************************************************************
(defun printSquares(board)
  (cond
    ((null board)
    )
    (t
      (princ " ")
      (cond
        ((= (first board) 0)
          (princ '*)
        )
        (t
          (princ (first board))
        )
      )
      (printSquares (rest board))
    )
  )
)

;********************************************************************* 
;Function Name: printBoard
;Purpose: To print both the human's and computer's board to the screen
;Parameters: 
;            humanBoard, a list of integers. The human's board
;            computerBoard, a list of integers. The computer's board
;Return Value: True
;Local Variables: None
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun printBoard(humanBoard computerBoard)
  (princ "Human's Board:   ") (printSquares humanBoard) (terpri)
  (princ "Computer's Board:") (printSquares computerBoard) (terpri)
)

;********************************************************************* 
;Function Name: getRoll
;Purpose: To get a single roll from the file with the die rolls in it
;Parameters: None
;Return Value: NIL if the there was no more rolls in file, The die roll value if there was at least one roll in the file
;Local Variables:
;                 1) roll, a list of integers. All the rolls read from the file
;Algorithm:
;           1) Read in the rolls from the file
;           2) Save the rest of the rolls to the file
;           3) Return the first of the rolls
;Assistance Received: none 
;*********************************************************************
(defun getRoll()
  (let* ((rolls (readRoll)))
    (saveRolls (rest rolls))
    (first rolls)
  )
)

;********************************************************************* 
;Function Name: dieRoll
;Purpose: To get a single die roll
;Parameters: None
;Return Value: A number between 1-6
;Local Variables:
;                 1) roll, a list of integers. A roll from the dice file
;Algorithm:
;           1) If there are no rolls in the file, return a random die roll
;           2) Else return the roll from the file
;Assistance Received: none 
;*********************************************************************
(defun dieRoll()
  (let* ((roll (getRoll)))
    (cond
      ((null roll)
        (+ (random 6) 1)
      )
      (t
        roll
      )
    )
  )
)

;********************************************************************* 
;Function Name: initialRoll
;Purpose: To determine who goes first
;Parameters: None
;Return Value: True if human goes first, NIL if computer goes first
;Local Variables:
;                 1) human, an integer. The number of pips the human rolled
;                 2) computer, an integer. The number of pips the computer rolled
;Algorithm:
;           1) If human and computer rolled the same number, recurse into this function
;           2) If human rolled more pips, return T
;           3) Else computer rolled more pips, return NIL
;Assistance Received: none 
;*********************************************************************
(defun initialRoll()
  (let ((human (+ (dieRoll) (dieRoll))) (computer (+ (dieRoll) (dieRoll))))
    (princ "Human's Rolled:    ") (princ human) (terpri)
    (princ "Computer's Rolled: ") (princ computer) (terpri)
    (cond
      ((= human computer)
        (princ "Rerolling...") (terpri)
        (terpri)
        (initialRoll)
      )
      ((> human computer)
        (princ "Human goes first.") (terpri)
        (terpri)
        T
      )
      (t
        (princ "Computer goes first.") (terpri)
        (terpri)
        nil
      )
    )
  )
)

;********************************************************************* 
;Function Name: setSize
;Purpose: To allow the user to set the size of the board
;Parameters: None
;Return Value: 9, 10, or 11
;Local Variables:
;                 1) size, an integer. The number the user entered
;Algorithm:
;           1) If the number is 9, 10, or 11, return the number
;           2) Else, recurse into the function
;Assistance Received: none 
;*********************************************************************
(defun setSize()
  (princ "Enter board size [9-11]: ")
  (let* ((size (read)))
      (terpri)
      (cond
        ((= size 9)
          size
        )
        ((= size 10)
          size
        )
        ((= size 11)
          size
        )
        (t
          (princ "Invalid size.") (terpri)
          (terpri)
          (setSize)
        )
      )
  )
)

;********************************************************************* 
;Function Name: newBoard
;Purpose: To create a board of size 9, 10, 11
;Parameters:
;             size, an integer. The size of the board
;Return Value: A list of integers from 1-size
;Local Variables: None
;Algorithm:
;           1) If the size is 9, 10, or 11, return a list of integers from 1-size
;           2) Else, return an empty list
;Assistance Received: none 
;*********************************************************************
(defun newBoard(size)
  (cond
    ((= size 9)
      '(1 2 3 4 5 6 7 8 9)
    )
    ((= size 10)
      '(1 2 3 4 5 6 7 8 9 10)
    )
    ((= size 11)
      '( 1 2 3 4 5 6 7 8 9 10 11)
    )
    (t
      '()
    )
  )
)

;********************************************************************* 
;Function Name: sum
;Purpose: To sum a list of integers
;Parameters:
;             combination, a list of integer. The list of numbers to sum together
;Return Value: The sum of all the integers in the list
;Local Variables: None
;Algorithm:
;           1) If the combination is empty, return 0
;           2) Else, recurse into the function with the rest of the combination, return the value from the recursed function plus the first element in combination
;Assistance Received: none 
;*********************************************************************
(defun sum(combination)
  (cond
    ((null combination)
      0
    )
    (t
      (+ (first combination) (sum (rest combination)))
    )
  )
)

;********************************************************************* 
;Function Name: cover_uncover
;Purpose: To prompt the user to cover or uncover
;Parameters:
;             humanBoard, a list of integer. The human's board
;             computerBoard, a list of integer. The computer's board
;Return Value: 1 if the user chose to cover, 2 if the user chose to uncover
;Local Variables:
;                 input, an integer. The option the user chose
;Algorithm:
;           1) If the user chose 0, display the hint and recurse into this function
;           2) If the user chose 1, return 1
;           3) If the user chose 1, return 2
;           4) Else, recurse into this function
;Assistance Received: none 
;*********************************************************************
(defun cover_uncover(humanBoard computerBoard)
  (terpri)
  (princ "0) Hint") (terpri)
  (princ "1) Cover") (terpri)
  (princ "2) Uncover") (terpri)
  (princ "Enter option: ")
  (let* ((input (read)))
    (cond
      ((= input 0)
        (princ "Hint: ") (coverHint humanBoard computerBoard) (terpri)
        (cover_uncover humanBoard computerBoard)
      )
      ((= input 1)
        1
      )
      ((= input 2)
        2
      )
      (t
        (princ "Invaild input.") (terpri)
        (cover_uncover humanBoard computerBoard)
      )
    )
  )
)

;********************************************************************* 
;Function Name: getSquare
;Purpose: To get the value of a square
;Parameters:
;             square, an integer. The square to return the value of
;             board, a list of integer. The board to check
;             count, an integer. The number of iterations of this function
;Return Value: -1 of the square isn't within the board, the value of the square if it is within the board
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return -1 [User entered invalid parameters]
;           2) If the square and the count are equal, return the first element in the board
;           3) Else, recurse into this function with the rest of the board and an incremented count
;Assistance Received: none 
;*********************************************************************
(defun getSquare(square board count)
  (cond
    ((null board)
      -1
    )
    (t
      (cond
        ((= square count)
          (first board)
        )
        (t
          (getSquare square (rest board) (+ count 1))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: findXIn
;Purpose: To find a integer in a list
;Parameters:
;             number, an integer. The number to find in the list
;             combination, a list of integer. The list to search through
;Return Value: T if the number is in the list, NIL if its not
;Local Variables: None
;Algorithm:
;           1) If the combination is empty, return NIL
;           2) If the first element in the combination is equal to the number, return T
;           3) Else, recurse into this function with the rest of the combination
;Assistance Received: none 
;*********************************************************************
(defun findXIn(number combination)
  (cond
    ((null combination)
      nil
    )
    (t
      (cond
        ((= number (first combination))
          t
        )
        (t
          (findXIn number (rest combination))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: playerCover
;Purpose: To allow the user to enter a series of square to covered
;Parameters:
;             rollTotal, an integer. The number of pips rolled
;             board, a list of integer. The list to search through
;             combination, a list of integer. A list of valid square the user wishes to cover
;Return Value: The list of squares the user wishes to cover
;Local Variables:
;                 input, user input integer. A square the user wishes to cover
;Algorithm:
;           1) If the sum of the combination is greater than the rollTotal, recurse into this function with a empty list as combination
;           2) If the sum of the combination is equal to the rollTotal, return the combination
;           3) Else, prompt the user for input
;             4) If the input is greater than the size of the board or less than 0, recurse into this function with the same parameters
;             5) If the input is equal to 0, display the hint and recurse into this function with the same parameters
;             6) If the square that was input is equal to the number that was input
;               7) If the number input is already in the combination, recurse into this function with the same parameters
;               8) Else, add the input number to the front of the combination and recurse into this function with the new combination
;             9) Else, recurse into this function with the same parameters
;Assistance Received: none 
;*********************************************************************
(defun playerCover(rollTotal board combination)
  (terpri)
  (printSquares board) (terpri)
  (terpri)
  (princ "Total Rolled: ") (princ rollTotal) (terpri)
  (princ "Square chosen: ") (princ combination) (terpri)
  (cond
    ((> (sum combination) rollTotal)
      (princ "Invalid combination.") (terpri)
      (princ "Try again.") (terpri)
      (terpri)
      (playerCover rollTotal board '())
    )
    ((= (sum combination) rollTotal)
      (princ "Combination Accepted.") (terpri)
      (terpri)
      combination
    )
    (t
      (princ "Enter a square to cover(Hint = 0): ")
      (let* ((input (read)))
        (cond
          ((or (> input (getSize board)) (< input 0))
            (princ "Invalid square.") (terpri)
            (terpri)
            (playerCover rollTotal board combination)
          )
          ((= input 0)
            (princ "Hint: ") (printSquares (getBestCoverMove rollTotal board)) (terpri)
            (playerCover rollTotal board combination)
          )
          (t
            ;Valid Square
            (cond
              ((= (getSquare input board 1) input)
                ;Uncovered Square
                (cond
                  ((findXIn input combination)
                    (princ input) (princ " is already getting covered.") (terpri)
                    (playerCover rollTotal board combination)
                  )
                  (t
                    (playerCover rollTotal board (cons input combination))
                  )
                )
              )
              (t
                ;Covered Square
                (princ input) (princ " is already covered.") (terpri)
                (playerCover rollTotal board combination)
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: playerUncover
;Purpose: To allow the user to enter a series of squares to uncovered
;Parameters:
;             rollTotal, an integer. The number of pips rolled
;             board, a list of integer. The list to search through
;             combination, a list of integer. A list of valid square the user wishes to uncover
;Return Value: The list of squares the user wishes to uncover
;Local Variables:
;                 input, user input integer. A square the user wishes to uncover
;Algorithm:
;           1) If the sum of the combination is greater than the rollTotal, recurse into this function with a empty list as combination
;           2) If the sum of the combination is equal to the rollTotal, return the combination
;           3) Else, prompt the user for input
;             4) If the input is greater than the size of the board or less than 0, recurse into this function with the same parameters
;             5) If the input is equal to 0, display the hint and recurse into this function with the same parameters
;             6) If the square that was input is equal to the number that was input
;               7) If the number input is already in the combination, recurse into this function with the same parameters
;               8) Else, add the input number to the front of the combination and recurse into this function with the new combination
;             9) Else, recurse into this function with the same parameters
;Assistance Received: none 
;*********************************************************************
(defun playerUncover(roll board combination)
  (terpri)
  (printSquares board) (terpri)
  (terpri)
  (princ "Total Rolled: ") (princ roll) (terpri)
  (princ "Square chosen: ") (princ combination) (terpri)
  (cond
    ((> (sum combination) roll)
      (princ "Invalid combination.") (terpri)
      (princ "Try again.") (terpri)
      (terpri)
      (playerUncover roll board '())
    )
    ((= (sum combination) roll)
      (princ "Combination Accepted.") (terpri)
      (terpri)
      combination
    )
    (t
      (princ "Enter a square to uncover(Hint = 0): ")
      (let* ((input (read)))
        (cond
          ((or (> input (getSize board)) (< input 0))
            (princ "Invalid square.") (terpri)
            (terpri)
            (playerUncover roll board combination)
          )
          ((= input 0)
            (princ "Hint: ") (printSquares (getBestUncoverMove roll board)) (terpri)
            (playerUncover roll board combination)
          )
          (t
            ;Valid Square
            (cond
              ((= (getSquare input board 1) 0)
                ;covered Square
                (cond
                  ((findXIn input combination)
                    (princ input) (princ " is already getting uncovered.") (terpri)
                    (playerUncover roll board combination)
                  )
                  (t
                    (playerUncover roll board (cons input combination))
                  )
                )
              )
              (t
                ;uncovered Square
                (princ input) (princ " is already uncovered.") (terpri)
                (playerUncover roll board combination)
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: 7UpCovered
;Purpose: To check if the 7-n square are covered
;Parameters:
;             board, a list of integer. The list to search through
;             i, an integer. The number of iterations through the function
;Return Value: T if square 7-n are covered, NIL if at least one square between 7-n is uncovered
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return T
;           2) If the rest of the board also returns T
;             3) If i is greater than or equal to 7
;               4) If the first element in the board is equal to 0, return T
;               5) Else, return NIL
;             6) Else, return T
;           7) Else, return NIL
;Assistance Received: none 
;*********************************************************************
(defun 7UpCovered(board i)
  (cond
    ((null board)
      t
    )
    (t
      (cond
        ((7UpCovered (rest board) (+ i 1))
          (cond
            ((>= i 7)
              (cond
                ((= (first board) 0)
                  t
                )
                (t
                  nil
                )
              )
            )
            (t
              t
            )
          )
        )
        (t
          nil
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: chooseDice
;Purpose: To allow the user to choose to roll a die or dice
;Parameters: None
;Return Value: 1 if the user chose to roll a die, 2 if the user chose to roll dice
;Local Variables:
;                 input, user input integer. The option the user chose
;Algorithm:
;           1) If the user entered 1, return 1
;           2) If the user entered 2, return 2
;           3) Else, recurse into this function
;Assistance Received: none 
;*********************************************************************
(defun chooseDice()
  ;(princ "Square 7-n are covered.") (terpri)
  ;(terpri)
  (princ "Roll Options: ") (terpri)
  (princ "--------------") (terpri)
  (princ "1) Roll die   ") (terpri)
  (princ "2) Roll dice  ") (terpri)
  (princ "Select option: ")
  (let* ((input (read)))
    (cond
      ((= input 1)
        1
      )
      ((= input 2)
        2
      )
      (t
        (princ "Invalid input.") (terpri)
        (terpri)
        (chooseDice)
      )
    )
  )
)

;********************************************************************* 
;Function Name: allCovered
;Purpose: To check if a entire board is covered
;Parameters:
;             board, a list of integers. The board to search through
;Return Value: T if all the square in board are covered. NIL if at least one square is uncovered.
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return T
;           2) If the rest of the board returned T and the first element in board is equal to 0, return T
;           3) Else, return NIL
;Assistance Received: none 
;*********************************************************************
(defun allCovered(board)
  (cond
    ((null board)
      t
    )
    (t
      (cond
        ((and (= (first board) 0) (allCovered (rest board)))
          t
        )
        (t
          nil
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: allUncovered
;Purpose: To check if a entire board is uncovered
;Parameters:
;             board, a list of integers. The board to search through
;             i, an integer. The number of iterations through this function
;Return Value: T if all the square in board are uncovered. NIL if at least one square is covered.
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return T
;           2) If the rest of the board returned T and the first element in board is equal to i, return T
;           3) Else, return NIL
;Assistance Received: none 
;*********************************************************************
(defun allUncovered(board i)
  (cond
    ((null board)
      t
    )
    (t
      (cond
        ((and (= (first board) i) (allUncovered (rest board) (+ i 1)))
          t
        )
        (t
          nil
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: checkWin
;Purpose: To check if the game is over
;Parameters:
;             myBoard, a list of integers. The board owned by the player calling this function
;             theirBoard, a list of integers. The board owned by the player's opponent
;Return Value: T if all the square on the player's board are covered or if all the squares on the opponent's board are uncovered, NIL otherwise
;Local Variables: None
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun checkWin(myBoard theirBoard)
  (cond
    ((or (allCovered myBoard) (allUncovered theirBoard 1))
      t
    )
    (t
      nil
    )
  )
)

;********************************************************************* 
;Function Name: humanMove
;Purpose: To allow the user to make a single move within their turn
;Parameters:
;             roll, an integers. The number of pips rolled by the user
;             humanBoard, a list of integers. The board owned by the user
;             computerBoard, a list of integers. The board owned by the computer
;Return Value: An empty list of the human can't make a move, A list containing the modified human and computer board
;Local Variables: None
;Algorithm:
;           1) If the human chose to cover
;             2) If the user has no possible cover moves
;               3) If the user has no possible uncover moves, return an empty list
;               4) Else, return the list the user wishes to uncover
;             5) Else, return the list the user wishes to cover
;           6) Else
;             7) If the user has no possible uncover moves
;               8) If the user has no possible cover moves, return an empty list
;               9) Else, return the list the user wishes to cover
;             10) Else, return the list the user wishes to uncover
;Assistance Received: none 
;*********************************************************************
(defun humanMove(roll humanBoard computerBoard)
  (terpri)
  (princ "Rolled: ") (princ roll) (terpri)
  (cond
    ((= (cover_uncover humanBoard computerBoard) 1)
      (cond
        ((null (getBestCoverMove roll humanBoard))
          (princ "No possible cover moves.") (terpri)
          (cond
            ((null (getBestUncoverMove roll computerBoard))
              (princ "No possible uncover moves.") (terpri)
              '()
            )
            (t
              (princ "Uncover move available.") (terpri)
              (princ "Switched to uncover move!!!") (terpri)
              (cons humanBoard (list (uncoverCombo (playerUncover roll computerBoard ()) computerBoard)))
            )
          )
        )
        (t
          (cons (coverCombo (playerCover roll humanBoard ()) humanBoard) (list computerBoard))
        )
      )
    )
    (t
      ;player chose to uncover
      (cond
        ((null (getBestUncoverMove roll computerBoard))
          (princ "No possible uncover moves.") (terpri)
          (cond
            ((null (getBestCoverMove roll humanBoard))
              (princ "No possible cover moves.") (terpri)
              '()
            )
            (t
              (princ "Cover move available.") (terpri)
              (princ "Switched to cover move!!!") (terpri)
              (cons (coverCombo (playerCover roll humanBoard ()) humanBoard) (list computerBoard))
            )
          )
        )
        (t
          (cons humanBoard (list(uncoverCombo (playerUncover roll computerBoard ()) computerBoard)))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: humanTurn
;Purpose: To allow the user to take multiple moves within their turn
;Parameters:
;             canWin, boolean. Determines if the user can win
;             humanBoard, a list of integers. The board owned by the player
;             computerBoard, a list of integers. The board owned by the computer
;Return Value: A list containing an updated human and computer board
;Local Variables:
;                 choice, an integer [1 or 2]. The number of dice the user chose to roll
;                 move, a list of integers. The move the user chose to make
;Algorithm:
;           1) If the user's 7-n square are covered
;             2) If the user chose to roll a die
;               3) If the move is empty, return the boards from the parameters
;               4) If the user won, return the boards the user modified
;               5) Else, recurse into this function with the user modified boards
;             6) Else [roll dice]
;               7) If the move is empty, return the boards from the parameters
;               8) If the user won, return the boards the user modified
;               9) Else, recurse into this function with the user modified boards
;           10) Else [roll dice]
;             11) If the move is empty, return the boards from the parameters
;             12) If the user won, return the boards the user modified
;             13) Else, recurse into this function with the user modified boards
;Assistance Received: none 
;*********************************************************************
(defun humanTurn(canWin humanBoard computerBoard)
  (cond
    ((7UpCovered humanBoard 1)
      (let* ((choice (chooseDice)))
        (cond
          ((= choice 1)
            ;(princ "Player chose to roll die.") (terpri)
            (let* ((move (humanMove (dieRoll) humanBoard computerBoard)))
              (cond
                ((null move)
                  (cons humanBoard (list computerBoard))
                )
                ((and (checkWin (first move) (first (rest move))) canWin)
                  (terpri)
                  (printBoard (first move) (first (rest move)))
                  (terpri)
                  (cons (first move) (list (first (rest move))))
                )
                (t
                  (terpri)
                  (printBoard (first move) (first (rest move)))
                  (terpri)
                  (humanTurn canWin (first move) (first (rest move)))
                )
              )
            )
          )
          (t
            ;(princ "Player chose to roll dice.") (terpri)
            (let* ((move (humanMove (+ (dieRoll) (dieRoll)) humanBoard computerBoard)))
              (cond
                ((null move)
                  (cons humanBoard (list computerBoard))
                )
                ((and (checkWin (first move) (first (rest move))) canWin)
                  (terpri)
                  (printBoard (first move) (first (rest move)))
                  (terpri)
                  (cons (first move) (list (first (rest move))))
                )
                (t
                  (terpri)
                  (printBoard (first move) (first (rest move)))
                  (terpri)
                  (humanTurn canWin (first move) (first (rest move)))
                )
              )
            )
          )
        )
      )
    )
    (t
      ;(princ "Automated roll dice") (terpri)
      (let* ((move (humanMove (+ (dieRoll) (dieRoll)) humanBoard computerBoard)))
        (cond
          ((null move)
            (cons humanBoard (list computerBoard))
          )
          ((and (checkWin (first move) (first (rest move))) canWin)
            (terpri)
            (printBoard (first move) (first (rest move)))
            (terpri)
            (cons (first move) (list (first (rest move))))
          )
          (t
            (terpri)
            (printBoard (first move) (first (rest move)))
            (terpri)
            (humanTurn canWin (first move) (first (rest move)))
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: computerMove
;Purpose: To allow the computer to make a single move within its turn
;Parameters:
;             roll, an integers. The number of pips rolled by the computer
;             humanBoard, a list of integers. The board owned by the user
;             computerBoard, a list of integers. The board owned by the computer
;Return Value: An empty list of the computer can't make a move, A list containing the modified human and computer board
;Local Variables: None
;Algorithm:
;           1) If the computer chose to cover
;             2) If the computer has no possible cover moves
;               3) If the computer has no possible uncover moves, return an empty list
;               4) Else, return the list the computer wishes to uncover
;             5) Else, return the list the computer wishes to cover
;           6) Else
;             7) If the computer has no possible uncover moves
;               8) If the computer has no possible cover moves, return an empty list
;               9) Else, return the list the computer wishes to cover
;             10) Else, return the list the computer wishes to uncover
;Assistance Received: none 
;*********************************************************************
(defun computerMove(roll humanBoard computerBoard)
  (terpri)
  (princ "Rolled: ") (princ roll) (terpri)
  (cond
    ((<= (determineCover computerBoard humanBoard) 1)
      (princ "More points by covering.") (terpri)
      (princ "Covering...") (terpri)
      (cond
        ((null (getBestCoverMove roll computerBoard))
          (princ "No possible cover moves.") (terpri)
          (cond
            ((null (getBestUncoverMove roll humanBoard))
              (princ "No possible uncover moves.") (terpri)
              '()
            )
            (t
              (princ "Uncover move available.") (terpri)
              (princ "Uncovering...") (terpri)
              (princ "Computer's Move: ") (princ (getBestUncoverMove roll humanBoard)) (princ " is highest combination of squares.") (terpri)
              (cons (uncoverCombo (getBestUncoverMove roll humanBoard) humanBoard) (list computerBoard))
            )
          )
        )
        (t
          ;cover
          (princ "Computer's Move: ") (princ (getBestCoverMove roll computerBoard)) (princ " is highest combination of squares.") (terpri)
          (cons humanBoard (list (coverCombo (getBestCoverMove roll computerBoard) computerBoard)))
        )
      )
    )
    (t
      ;player chose to uncover
      (princ "More points by uncovering.") (terpri)
      (princ "Uncovering...") (terpri)
      (cond
        ((null (getBestUncoverMove roll humanBoard))
          (princ "No possible uncover moves.") (terpri)
          (cond
            ((null (getBestCoverMove roll computerBoard))
              (princ "No possible cover moves.") (terpri)
              '()
            )
            (t
              (princ "Cover move available.") (terpri)
              (princ "Covering...") (terpri)
              (princ "Computer's Move: ") (princ (getBestCoverMove roll computerBoard)) (princ " is highest combination of squares.") (terpri)
              (cons humanBoard (list (coverCombo (getBestCoverMove roll computerBoard) computerBoard)))
            )
          )
        )
        (t
          (princ "Computer's Move: ") (princ (getBestUncoverMove roll humanBoard)) (princ " is highest combination of squares.") (terpri)
          (cons (uncoverCombo (getBestUncoverMove roll humanBoard) humanBoard) (list computerBoard))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: computerTurn
;Purpose: To allow the computer to take multiple moves within their turn
;Parameters:
;             canWin, boolean. Determines if the computer can win
;             humanBoard, a list of integers. The board owned by the player
;             computerBoard, a list of integers. The board owned by the computer
;Return Value: A list containing an updated human and computer board
;Local Variables:
;                 move, a list of integers. The move the user chose to make
;Algorithm:
;           1) If the computer's 7-n square are covered and the computer wants to cover [roll die]
;             2) If the move is empty, return the board in the parameters
;             3) If the computer won, return a list containing the modified human and computer board
;             4) Else, recurse into this function with the modified human and computer board
;           5) Else [roll dice]
;             6) If the move is empty, return the board in the parameters
;             7) If the computer won, return a list containing the modified human and computer board
;             8) Else, recurse into this function with the modified human and computer board
;Assistance Received: none 
;*********************************************************************
(defun computerTurn(canWin humanBoard computerBoard) ;returns computers series of moves
  (cond
    ((and (7UpCovered computerBoard 1) (<= (determineCover computerBoard humanBoard) 1))
      (princ "Rolled die.") (terpri)
      (let* ((move (computerMove (dieRoll) humanBoard computerBoard)))
        (cond
          ((null move)
            (cons humanBoard (list computerBoard))
          )
          ((and (checkWin (first (rest move)) (first move)) canWin)
            ;(princ "Computer's Move: ") (princ (first move)) (princ (first (rest move))) (terpri)
            (terpri)
            (printBoard (first move) (first (rest move)))
            (terpri)
            (cons (first move) (list (first (rest move))))
          )
          (t
            ;(princ "Computer's Move: ") (princ (first move)) (princ (first (rest move))) (terpri)
            (terpri)
            (printBoard (first move) (first (rest move)))
            (terpri)
            (computerTurn canWin (first move) (first (rest move)))
          )
        )
      )
    )
    (t
      (princ "Rolled dice.") (terpri)
      (let* ((move (computerMove (+ (dieRoll) (dieRoll)) humanBoard computerBoard)))
        (cond
          ((null move)
            (cons humanBoard (list computerBoard))
          )
          ((and (checkWin (first (rest move)) (first move)) canWin)
            ;(princ "Computer's Move: ") (princ (first move)) (princ (first (rest move))) (terpri)
            (terpri)
            (printBoard (first move) (first (rest move)))
            (terpri)
            (cons (first move) (list (first (rest move))))
          )
          (t
            ;(princ "Computer's Move: ") (princ (first move)) (princ (first (rest move))) (terpri)
            (terpri)
            (printBoard (first move) (first (rest move)))
            (terpri)
            (computerTurn canWin (first move) (first (rest move)))
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: calcScore
;Purpose: To calculate the winners score
;Parameters:
;             myBoard, a list of integers. The board owned by the player calling this function
;             theirBoard, a list of integers. The board owned by the player's opponent
;Return Value: The score of the winner
;Local Variables: None.
;Algorithm:
;           1) If all of the player's pieces are covered, return the sum of all the opponenet's uncovered squares
;           2) Else, return the sum of all the player's covered squares
;Assistance Received: none 
;*********************************************************************
(defun calcScore(myBoard theirBoard)
  (cond
    ((allCovered myBoard)
      (princ "Won by covering.") (terpri)
      ;sum all their uncovered pieces
      (sum (getUncoveredSquares theirBoard 1))
    )
    (t
      (princ "Won by uncovering.") (terpri)
      ;sum all my covered pieces
      (sum (getCoveredSquares myBoard 1))
    )
  )
)

;********************************************************************* 
;Function Name: beginTurns
;Purpose: To alternate between the human's and computer's turn
;Parameters:
;             humanFirst, a boolean. The value of if the human went first
;             humanScore, an integer. The human's score
;             computerScore, an integer. The computer's score
;             humanBoard, a list of integers. The human's board
;             computerBoard, a list of integers. The computer's board
;Return Value: A list containing the human's and computer's score
;Local Variables:
;                 board, a list of two lists of integers. The modified human and computer board
;                 newHumanBoard, a list of integers. The modified human board
;                 newComputerBoard, a list of integers. The modified computer board
;                 newBoard, a list of two lists of integers. The modified newHumanBoard and newComputerBoard
;                 newNewHumanBoard, a list of integers. The modified newHumanBoard
;                 newNewComputerBoard, a list of integers. The modified newComputerBoard
;Algorithm:
;           1) If the human went first
;             2) If the computer won, return a list of 0 and the computer's score
;             3) If the user wishes to save, save the game
;             4) Else
;               5) If the human won, return a list of the human's score and 0
;               6) If the user wishes to save, save the game
;               7) Else, recurse into this function with the updated human and computer boards
;           8) Else
;             9) If the human won, return a list of the human's score and 0
;             10) If the user wishes to save, save the game
;             11) Else
;               12) If the computer won, return a list of 0 and the computer's score
;               13) If the user wishes to save, save the game
;               14) Else, recurse into this function with the updated human and computer boards
;Assistance Received: none 
;*********************************************************************
(defun beginTurns(humanFirst humanScore computerScore humanBoard computerBoard)
  (cond
    (humanFirst ;human had first turn
      (princ "--------------------") (terpri)
      (princ "Computer's Turn     ") (terpri)
      (let* ((board (computerTurn t humanBoard computerBoard)))
        (let* ((newHumanBoard (first board)) (newComputerBoard (first (rest board))))
          (princ "Computer's Turn Over") (terpri)
          (princ "--------------------") (terpri)
          (cond
            ((checkWin newComputerBoard newHumanBoard)
              (terpri)
              (princ "Computer Wins.") (terpri)
              (cons 0 (list (calcScore newComputerBoard newHumanBoard)))
            )
            ((promptSave)
              (saveGame newHumanBoard humanScore newComputerBoard computerScore humanFirst t)
            )
            (t
              (terpri)
              (princ "------------") (terpri)
              (princ "Human's Turn") (terpri)
              (let* ((newBoard (humanTurn t newHumanBoard newComputerBoard)))
                (let* ((newNewHumanBoard (first newBoard)) (newNewComputerBoard (first (rest newBoard))))
                  (princ "Human's Turn Over") (terpri)
                  (princ "-----------------") (terpri)
                  (cond
                    ((checkWin newNewHumanBoard newNewComputerBoard)
                      (terpri)
                      (princ "Human Wins.") (terpri)
                      (cons (calcScore newNewHumanBoard newNewComputerBoard) (list 0))
                    )
                    ((promptSave)
                      (saveGame newNewHumanBoard humanScore newNewComputerBoard computerScore humanFirst nil)
                    )
                    (t
                      (beginTurns humanFirst humanScore computerScore newNewHumanBoard newNewComputerBoard)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    (t ;computer had first turn
      (princ "-----------------") (terpri)
      (princ "Human's Turn     ") (terpri)
      (let* ((board (humanTurn t humanBoard computerBoard)))
        (let* ((newHumanBoard (first board)) (newComputerBoard (first (rest board))))
          (princ "Human's Turn Over") (terpri)
          (princ "-----------------") (terpri)
          (cond
            ((checkWin newHumanBoard newComputerBoard)
              (terpri)
              (princ "Human Wins.") (terpri)
              (cons (calcScore newHumanBoard newComputerBoard) (list 0))
            )
            ((promptSave)
              (saveGame newHumanBoard humanScore newComputerBoard computerScore humanFirst nil)
            )
            (t
              (terpri)
              (princ "--------------------") (terpri)
              (princ "Computer's Turn     ") (terpri)
              (let* ((newBoard (computerTurn t newHumanBoard newComputerBoard)))
                (let* ((newNewHumanBoard (first newBoard)) (newNewComputerBoard (first (rest newBoard))))
                  (princ "Computer's Turn Over") (terpri)
                  (princ "--------------------") (terpri)
                  (cond
                    ((checkWin newNewComputerBoard newNewHumanBoard)
                      (terpri)
                      (princ "Computer Wins.") (terpri)
                      (cons 0 (list (calcScore newNewComputerBoard newNewHumanBoard)))
                    )
                    ((promptSave)
                      (saveGame newNewHumanBoard humanScore newNewComputerBoard computerScore humanFirst t)
                    )
                    (t
                      (beginTurns humanFirst humanScore computerScore newNewHumanBoard newNewComputerBoard)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: playAgain
;Purpose: To prompt the user to play another game
;Parameters: None
;Return Value: T if yes, NIL if no
;Local Variables:
;                 input, user input integer. The option the user chose
;Algorithm: 
;           1) If input equals 1, return T
;           2) If input equals 2, return NIL
;           3) Else, recurse into this function
;Assistance Received: none 
;*********************************************************************
(defun playAgain()
  (princ "Play Again?") (terpri)
  (princ "1) Yes") (terpri)
  (princ "2) No") (terpri)
  (princ "Enter input: ")
  (let* ((input (read)))
    (cond
      ((= input 1)
        t
      )
      ((= input 2)
        nil
      )
      (t
        (princ "Invalid input.") (terpri)
        (terpri)
        (playAgain)
      )
    )
  )
)

;********************************************************************* 
;Function Name: addHandicap
;Purpose: To add a handicap to a board
;Parameters:
;             winnersScore, an integer. The winner's score
;             board, a list of integers. The board to apply the handicap
;Return Value: The board with a handicap applied
;Local Variables:
;                 sumScore, an integer. The digits of the winner's score summed together
;Algorithm: 
;           1) If the winner's score is greater than 9
;             2) If the summed Score is greater than 9, return the board with the handicap on square sumScore-9
;             3) Else, return the board with the handicap on square sumScore
;           4) Else, return the board with the handicap on square winnersScore
;Assistance Received: none 
;*********************************************************************
(defun addHandicap(winnersScore board)
  (cond
    ((> winnersScore 9)
      (let* ((sumScore (+ (digit-char-p (char (write-to-string winnersScore) 0)) (digit-char-p (char (write-to-string winnersScore) 1)))))
        (cond
          ((> sumScore 9)
            (cover (- sumScore 9) board)
          )
          (t
            (cover sumScore board)
          )
        )
      )
    )
    (t
      (cover winnersScore board)
    )
  )
)

;********************************************************************* 
;Function Name: handicapGame
;Purpose: To play a game with a handicap
;Parameters:
;             humanHandicap, a boolean. Represents if the human gets the handicap
;             winnersScore, an integer. The winner's score
;             humanScore, an integer. The human's score
;             computerScore, an integer. The computer's score
;Return Value: A list containing the human's and computer's score
;Local Variables:
;                 size, an integer. The size of the board
;                 humanFirst, a boolean. Represents if the human goes first
;                 humanBoard, a list of integers. The human's board
;                 computerBoard, a list of integers. The computer's board
;                 board, a list of two lists of integers. The modified human and computer board
;                 newHumanBoard, a list of integers. The modified human board
;                 newComputerBoard, a list of integers. The modified computer board
;                 scores, a list of integers. The human's and computer's game score
;                 newHumanScore, an integer. The human's game score
;                 newComputerScore, an integer. The computer's game score
;                 tournamentScores, a list of integers. The human's and computer's tournament scores
;Algorithm: 
;           1) If the human gets the handicap
;             2) If the human goes first [human makes first turn]
;               3) If the user wants to play again
;                 4) If the human won; return a list of the new human's score and the computer's score
;                 5) Else, return a list of the human's score and the new computer's score
;               6) Else, return a list of the human's and computer's score
;             7) Else [computer makes first turn]
;               8) If the user wants to play again
;                 9) If the human won; return a list of the new human's score and the computer's score
;                 10) Else, return a list of the human's score and the new computer's score
;               11) Else, return a list of the human's and computer's score
;           12) Else [computer gets handicap]
;             13) If the human goes first [human makes first turn]
;               14) If the user wants to play again
;                 15) If the human won; return a list of the new human's score and the computer's score
;                 16) Else, return a list of the human's score and the new computer's score
;               17) Else, return a list of the human's and computer's score
;             18) Else [computer makes first turn]
;               19) If the user wants to play again
;                 20) If the human won; return a list of the new human's score and the computer's score
;                 21) Else, return a list of the human's score and the new computer's score
;               22) Else, return a list of the human's and computer's score
;Assistance Received: none 
;*********************************************************************
(defun handicapGame(humanHandicap winnersScore humanScore computerScore)
  (let* ((size (setSize)) (humanFirst (initialRoll)) (humanBoard (newBoard size)) (computerBoard (newBoard size)))
    (cond
      (humanHandicap
        (let* ((handiHumanBoard (addHandicap winnersScore humanBoard)))
          (printBoard handiHumanBoard computerBoard)
          (terpri)
          (cond
            (humanFirst
              (princ "-----------------") (terpri)
              (princ "Human's Turn     ") (terpri)
              (let* ((board (humanTurn nil handiHumanBoard computerBoard)))
                (let* ((newHumanBoard (first board)) (newComputerBoard (first (rest board)))) ;can't win on first turn
                  (princ "Human's Turn Over") (terpri)
                  (princ "-----------------") (terpri)
                  (let* ((scores (beginTurns humanFirst humanScore computerScore newHumanBoard newComputerBoard)))
                    (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
                      (terpri)
                      (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
                      (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
                      (terpri)
                      (cond
                        ((playAgain)
                          (cond
                            ((> newHumanScore newComputerScore)
                              ;Computer Handicap
                              (let* ((tournamentScores (handicapGame nil newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                            (t
                              ;Human Handicap
                              (let* ((tournamentScores (handicapGame t newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                          )
                        )
                        (t
                          (cons newHumanScore (list newComputerScore))
                        )
                      )
                    )
                  )
                )
              )
            )
            (t
              (princ "--------------------") (terpri)
              (princ "Computer's Turn     ") (terpri)
              (let* ((board (computerTurn nil '() computerBoard)))
                (let* ((newHumanBoard handiHumanBoard) (newComputerBoard (first (rest board)))) ;can't win on first turn
                  (princ "Computer's Turn Over") (terpri)
                  (princ "--------------------") (terpri)
                  (let* ((scores (beginTurns humanFirst humanScore computerScore newHumanBoard newComputerBoard)))
                    (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
                      (terpri)
                      (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
                      (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
                      (terpri)
                      (cond
                        ((playAgain)
                          (cond
                            ((> newHumanScore newComputerScore)
                              ;Computer Handicap
                              (let* ((tournamentScores (handicapGame nil newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                            (t
                              ;Human Handicap
                              (let* ((tournamentScores (handicapGame t newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                          )
                          ;(let* ((tournamentScores (newGame)))
                          ;  (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                          ;)
                        )
                        (t
                          (cons newHumanScore (list newComputerScore))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      (t
        (let* ((handiComputerBoard (addHandicap winnersScore computerBoard)))
          (printBoard humanBoard handiComputerBoard)
          (terpri)
          (cond
            (humanFirst
              (princ "-----------------") (terpri)
              (princ "Human's Turn     ") (terpri)
              (let* ((board (humanTurn nil humanBoard '())))
                (let* ((newHumanBoard (first board)) (newComputerBoard handiComputerBoard)) ;can't win on first turn
                  (princ "Human's Turn Over") (terpri)
                  (princ "-----------------") (terpri)
                  (let* ((scores (beginTurns humanFirst humanScore computerScore newHumanBoard newComputerBoard)))
                    (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
                      (terpri)
                      (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
                      (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
                      (terpri)
                      (cond
                        ((playAgain)
                          (cond
                            ((> newHumanScore newComputerScore)
                              ;Computer Handicap
                              (let* ((tournamentScores (handicapGame nil newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                            (t
                              ;Human Handicap
                              (let* ((tournamentScores (handicapGame t newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                          )
                        )
                        (t
                          (cons newHumanScore (list newComputerScore))
                        )
                      )
                    )
                  )
                )
              )
            )
            (t
              (princ "--------------------") (terpri)
              (princ "Computer's Turn     ") (terpri)
              (let* ((board (computerTurn nil humanBoard handiComputerBoard)))
                (let* ((newHumanBoard (first board)) (newComputerBoard (first (rest board)))) ;can't win on first turn
                  (princ "Computer's Turn Over") (terpri)
                  (princ "--------------------") (terpri)
                  (let* ((scores (beginTurns humanFirst humanScore computerScore newHumanBoard newComputerBoard)))
                  (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
                      (terpri)
                      (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
                      (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
                      (terpri)
                      (cond
                        ((playAgain)
                          (cond
                            ((> newHumanScore newComputerScore)
                              ;Computer Handicap
                              (let* ((tournamentScores (handicapGame nil newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                            (t
                              ;Human Handicap
                              (let* ((tournamentScores (handicapGame t newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                                (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                              )
                            )
                          )
                        )
                        (t
                          (cons newHumanScore (list newComputerScore))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: newGame
;Purpose: To play a new game
;Parameters: None
;Return Value: A list containing the human's and computer's score
;Local Variables:
;                 size, an integer. The size of the board
;                 humanFirst, a boolean. Represents if the human goes first
;                 humanBoard, a list of integers. The human's board
;                 computerBoard, a list of integers. The computer's board
;                 board, a list of two lists of integers. The modified human and computer board
;                 newHumanBoard, a list of integers. The modified human board
;                 newComputerBoard, a list of integers. The modified computer board
;                 scores, a list of integers. The human's and computer's game score
;                 newHumanScore, an integer. The human's game score
;                 newComputerScore, an integer. The computer's game score
;                 tournamentScores, a list of integers. The human's and computer's tournament scores
;Algorithm: 
;           1) If the human goes first [human makes first turn]
;             2) If the user wants to play again
;               3) If the human won; return a list of the new human's score and the computer's score
;               4) Else, return a list of the human's score and the new computer's score
;             5) Else, return a list of the human's and computer's score
;           6) Else [computer makes first turn]
;             7) If the user wants to play again
;               8) If the human won; return a list of the new human's score and the computer's score
;               9) Else, return a list of the human's score and the new computer's score
;             10) Else, return a list of the human's and computer's score
;Assistance Received: none 
;*********************************************************************
(defun newGame()
  (let* ((size (setSize)) (humanFirst (initialRoll)) (humanBoard (newBoard size)) (computerBoard (newBoard size)))
    (printBoard humanBoard computerBoard)
    (terpri)
    (cond
      (humanFirst
        (princ "-----------------") (terpri)
        (princ "Human's Turn     ") (terpri)
        (let* ((board (humanTurn nil humanBoard computerBoard)))
          (let* ((newHumanBoard (first board)) (newComputerBoard (first (rest board)))) ;can't win on first turn
            (princ "Human's Turn Over") (terpri)
            (princ "-----------------") (terpri)
            (let* ((scores (beginTurns humanFirst 0 0 newHumanBoard newComputerBoard)))
              (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
                (terpri)
                (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
                (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
                (terpri)
                (cond
                  ((playAgain)
                    (cond
                      ((> newHumanScore newComputerScore)
                        ;Computer Handicap
                        (let* ((tournamentScores (handicapGame nil newHumanScore newHumanScore newComputerScore)))
                          (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                        )
                      )
                      (t
                        ;Human Handicap
                        (let* ((tournamentScores (handicapGame t newComputerScore newHumanScore  newComputerScore)))
                          (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                        )
                      )
                    )
                  )
                  (t
                    (cons newHumanScore (list newComputerScore))
                  )
                )
              )
            )
          )
        )
      )
      (t
        (princ "--------------------") (terpri)
        (princ "Computer's Turn     ") (terpri)
        (let* ((board (computerTurn nil humanBoard computerBoard)))
          (let* ((newHumanBoard (first board)) (newComputerBoard (first (rest board)))) ;can't win on first turn
            (princ "Computer's Turn Over") (terpri)
            (princ "--------------------") (terpri)
            (let* ((scores (beginTurns humanFirst 0 0 newHumanBoard newComputerBoard)))
              (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
                (terpri)
                (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
                (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
                (terpri)
                (cond
                  ((playAgain)
                    (cond
                      ((> newHumanScore newComputerScore)
                        ;Computer Handicap
                        (let* ((tournamentScores (handicapGame nil newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                          (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                        )
                      )
                      (t
                        ;Human Handicap
                        (let* ((tournamentScores (handicapGame t newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                          (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                        )
                      )
                    )
                  )
                  (t
                    (cons newHumanScore (list newComputerScore))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: resumeGame
;Purpose: To play a previously saved game
;Parameters:
;             humanBoard, a list of integers. The human's board
;             humanScore, an integer. The human's tournament score
;             computerBoard, a list of integers. The computer's board
;             computerScore, an integer. The computer's tournament score
;             firstTurn, a boolean. Represents if the human gets the first turn
;             nextTurn, a boolean. Represents if the human gets the next turn
;Return Value: A list containing the human's and computer's score
;Local Variables:
;                 scores, a list of integers. The human's and computer's game score
;                 newHumanScore, an integer. The human's game score
;                 newComputerScore, an integer. The computer's game score
;                 tournamentScores, a list of integers. The human's and computer's tournament scores
;Algorithm: 
;           1) If the user wants to play again
;             2) If the human went first
;               3) If the human won, return a list of the new human's score and the new computer's score
;               4) Else, return a list of the new human's score and the new computer's score
;             5) Else [computer went first]
;               6) If the human won, return a list of the new human's score and the new computer's score
;               7) Else, return a list of the new human's score and the new computer's score
;           8) Else, return a list containing the updated human and computer score
;Assistance Received: none 
;*********************************************************************
(defun resumeGame(humanBoard humanScore computerBoard computerScore firstTurn nextTurn)
  (printBoard humanBoard computerBoard)
  (terpri)
  (let* ((scores (beginTurns (not nextTurn) humanScore computerScore humanBoard computerBoard)))
    (let* ((newHumanScore (first scores)) (newComputerScore (first (rest scores))))
      (terpri)
      (princ "Human's Scored: ") (princ newHumanScore) (princ " points")(terpri)
      (princ "Computer's Scored: ") (princ newComputerScore) (princ " points") (terpri)
      (terpri)
      (cond
        ((playAgain)
          (cond
            (firstTurn ; Human went first
              (cond
                ((> newHumanScore newComputerScore) ; Human won = Computer handicap, nil + human's score
                  ;Computer Handicap
                  (let* ((tournamentScores (handicapGame nil newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                    (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                  )
                )
                (t
                  ;Computer won = computer handicap
                  ;Human Handicap
                  (let* ((tournamentScores (handicapGame nil newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                    (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                  )
                )
              )
            )
            (t ; Computer went first
              (cond
                ((> newHumanScore newComputerScore)
                  ;Human won = Human handicap
                  ;Computer Handicap
                  (let* ((tournamentScores (handicapGame t newHumanScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                    (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                  )
                )
                (t
                  ;Computer won = human handicap
                  ;Human Handicap
                  (let* ((tournamentScores (handicapGame t newComputerScore (+ humanScore newHumanScore) (+ computerScore newComputerScore))))
                    (cons (+ newHumanScore (first tournamentScores)) (list (+ newComputerScore (first (rest tournamentScores)))))
                  )
                )
              )
            )
          )
        )
        (t
          (cons newHumanScore (list newComputerScore))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: mainMenuOptions
;Purpose: To handle the users main menu input
;Parameters: None
;Return Value: T
;Local Variables:
;                 input, an integer. The users option
;                 humanBoard, a list of integers. The human's board
;                 humanScore, an integer. The human's score
;                 computerBoard, a list of integers. The computer's board
;                 computerScore, an integer. The computer's score
;                 firstTurn, a boolean. Represents if the human goes first
;                 nextTurn, a boolean. Represents if the human goes next
;                 totals, a list of integers. The human's and computer's score
;                 newHumanScore, an integer. The human's score
;                 newComputerScore, an integer. The computer's score
;Algorithm: 
;           1) If the user selects a new game
;             2) If there was a tie, declare tie
;             3) If human won, declare human winner
;             4) Else, declare computer winner
;           5) If the user loaded a game
;             6) If the human when first
;               7) If the human went next
;                 8) If there was a tie, declare tie
;                 9) If human won, declare human winner
;                 10) Else, declare computer winner
;               11) Else [computer went next]
;                 12) If there was a tie, declare tie
;                 13) If human won, declare human winner
;                 14) Else, declare computer winner
;             15) Else [computer went first]
;                 16) If there was a tie, declare tie
;                 17) If human won, declare human winner
;                 18) Else, declare computer winner
;               19) Else [computer went next]
;                 20) If there was a tie, declare tie
;                 21) If human won, declare human winner
;                 22) Else, declare computer winner
;           5) If the user chose to exit, return T
;           5) Else, recurse into this function
;Assistance Received: none 
;*********************************************************************
(defun mainMenuOptions()
  (princ "Enter #: ")
  (let* ((input (read)))
    (terpri)
    (cond
      ((= input 1)
        (let* ((totals (newGame)))
          (terpri)
          (princ "Human's Total: ") (princ (first totals)) (terpri)
          (princ "Computer's Total: ") (princ (first (rest totals))) (terpri)
          (cond
            ((= (first totals) (first (rest totals)))
              (princ "Draw.") (terpri)
            )
            ((> (first totals) (first (rest totals)))
              (princ "Human Wins!!!") (terpri)
            )
            (t
              (princ "Computer Wins!!!") (terpri)
            )
          )
        )
      )
      ((= input 2)
        (princ "Enter a file name: ")
        (let* ((currentGame (loadGame (read))))
          ;(print currentGame)
          (terpri)
          (princ "Computer's Board:") (printSquares (first currentGame)) (terpri)
          (princ "Computer's Score: ") (princ (first (rest currentGame))) (terpri)
          (princ "Human's Board:   ") (printSquares (first (rest (rest currentGame)))) (terpri)
          (princ "Human's Score: ") (princ (first (rest (rest (rest currentGame))))) (terpri)
          (princ "First Turn: ") (princ (first (rest (rest (rest (rest currentGame)))))) (terpri)
          (princ "Next Turn: ") (princ (first (rest (rest (rest (rest (rest currentGame))))))) (terpri)
          (terpri)

          (let* ((humanBoard (first (rest (rest currentGame)))) (humanScore (first (rest (rest (rest currentGame))))) (computerBoard (first currentGame)) (computerScore (first (rest currentGame))) (firstTurn (first (rest (rest (rest (rest currentGame)))))) (nextTurn (first (rest (rest (rest (rest (rest currentGame))))))))
            (cond
              (firstTurn
                ; Human went first
                (cond
                  (nextTurn
                    ; Human goes next
                    (let* ((totals (resumeGame humanBoard humanScore computerBoard computerScore t t)))
                      (let* ((newHumanScore (+ humanScore (first totals))) (newComputerScore (+ computerScore (first (rest totals)))))
                        (terpri)
                        (princ "Human's Total: ") (princ newHumanScore) (terpri)
                        (princ "Computer's Total: ") (princ newComputerScore) (terpri)
                        (cond
                          ((= newHumanScore newComputerScore)
                            (princ "Draw.") (terpri)
                          )
                          ((> newHumanScore newComputerScore)
                            (princ "Human Wins!!!") (terpri)
                          )
                          (t
                            (princ "Computer Wins!!!") (terpri)
                          )
                        )
                      )
                    )
                  )
                  (t
                    ; Computer goes next
                    (let* ((totals (resumeGame humanBoard humanScore computerBoard computerScore t nil)))
                      (let* ((newHumanScore (+ humanScore (first totals))) (newComputerScore (+ computerScore (first (rest totals)))))
                        (terpri)
                        (princ "Human's Total: ") (princ newHumanScore) (terpri)
                        (princ "Computer's Total: ") (princ newComputerScore) (terpri)
                        (cond
                          ((= newHumanScore newComputerScore)
                            (princ "Draw.") (terpri)
                          )
                          ((> newHumanScore newComputerScore)
                            (princ "Human Wins!!!") (terpri)
                          )
                          (t
                            (princ "Computer Wins!!!") (terpri)
                          )
                        )
                      )
                    )
                  )
                )
              )
              (t
                ; Computer went first
                (cond
                  (nextTurn
                    ; Human goes next
                    (let* ((totals (resumeGame humanBoard humanScore computerBoard computerScore nil t)))
                      (let* ((newHumanScore (+ humanScore (first totals))) (newComputerScore (+ computerScore (first (rest totals)))))
                        (terpri)
                        (princ "Human's Total: ") (princ newHumanScore) (terpri)
                        (princ "Computer's Total: ") (princ newComputerScore) (terpri)
                        (cond
                          ((= newHumanScore newComputerScore)
                            (princ "Draw.") (terpri)
                          )
                          ((> newHumanScore newComputerScore)
                            (princ "Human Wins!!!") (terpri)
                          )
                          (t
                            (princ "Computer Wins!!!") (terpri)
                          )
                        )
                      )
                    )
                  )
                  (t
                    ; Computer goes next
                    (let* ((totals (resumeGame humanBoard humanScore computerBoard computerScore nil nil)))
                      (let* ((newHumanScore (+ humanScore (first totals))) (newComputerScore (+ computerScore (first (rest totals)))))
                        (terpri)
                        (princ "Human's Total: ") (princ newHumanScore) (terpri)
                        (princ "Computer's Total: ") (princ newComputerScore) (terpri)
                        (cond
                          ((= newHumanScore newComputerScore)
                            (princ "Draw.") (terpri)
                          )
                          ((> newHumanScore newComputerScore)
                            (princ "Human Wins!!!") (terpri)
                          )
                          (t
                            (princ "Computer Wins!!!") (terpri)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      ((= input 3)
        (princ "Exiting game...") (terpri)
      )
      (t
        (princ "Invalid input.") (terpri) (terpri)
        (mainMenuOptions)
      )
    )
  )
)

;********************************************************************* 
;Function Name: mainMenu
;Purpose: To display the mainMenu
;Parameters: None
;Return Value: T
;Local Variables: None
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun mainMenu()
  (terpri)
  (princ "   Canoga   ") (terpri)
  (princ "------------") (terpri)
  (princ "1) New Game ") (terpri)
  (princ "2) Load Game") (terpri)
  (princ "3) Exit Game") (terpri)
  (terpri)
  (mainMenuOptions)
)

;********************************************************************* 
;Function Name: getSize
;Purpose: To get the size of a board
;Parameters:
;             board, a list of integers. The board to get the size of
;Return Value: The size of the board
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return 0
;           2) Else, return the size of the rest of the board plus one
;Assistance Received: none 
;*********************************************************************
(defun getSize(board)
  (cond
    ((null board)
      0
    )
    (t
      (+ 1 (getSize (rest board)))
    )
  )
)

;********************************************************************* 
;Function Name: getCoveredSquares
;Purpose: To get a list of all the covered squares on a board
;Parameters:
;             board, a list of integers. The board to search through
;             i, an integers. The number of interations through this function
;Return Value: A list of the covered squares
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If the first element in the board is covered, return the list from the rest of the board with the first element
;           3) Else, return the list from the rest of the board
;Assistance Received: none 
;*********************************************************************
(defun getCoveredSquares(board i)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((= (first board) 0)
          (cons i (getCoveredSquares (rest board) (+ i 1)))
        )
        (t
          (getCoveredSquares (rest board) (+ i 1))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: getUncoveredSquares
;Purpose: To get a list of all the uncovered squares on a board
;Parameters:
;             board, a list of integers. The board to search through
;             i, an integers. The number of interations through this function
;Return Value: A list of the uncovered squares
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If the first element in the board is covered, return the list from the rest of the board
;           3) Else, return the list from the rest of the board with the first element
;Assistance Received: none 
;*********************************************************************
(defun getUncoveredSquares(board i)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((= (first board) 0)
          (getUncoveredSquares (rest board) (+ i 1))
        )
        (t
          (cons i (getUncoveredSquares (rest board) (+ i 1)))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: determineCover
;Purpose: To determine if it is better for the player to cover or uncover
;Parameters:
;             myBoard, a list of integers. The board owned by the player calling this function
;             theirBoard, a list of integers. The board owned by the player's opponent
;Return Value: 1 if better to cover, 2 if better to uncover, 0 if even
;Local Variables:
;                 mySum, an integer. The score the player would get by covering
;                 theirSum, an integer. The score the player would get by uncovering
;Algorithm:
;           1) If mySum and theirSum are equal, return 0
;           2) If mySum is greater than theirSum, return 1
;           3) Else, return 2
;Assistance Received: none 
;*********************************************************************
(defun determineCover(myBoard theirBoard)
  (let((mySum (sum(getCoveredSquares myBoard 1))) (theirSum (sum(getUncoveredSquares theirBoard 1))))
    (cond
      ((= mySum theirSum)
        0
      )
      ((> mySum theirSum)
        1
      )
      (t
        2
      )
    )
  )
)

;********************************************************************* 
;Function Name: coverHint
;Purpose: To display the cover/uncover hint to the user
;Parameters:
;            humanBoard, a list of integers. The human's board
;            computerBoard, a list of integers. The computer's board
;Return Value: T
;Local Variables:
;                 temp, an integer. The AI's suggestion
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun coverHint(humanBoard computerBoard)
  (let* ((temp (determineCover humanBoard computerBoard)))
    (cond
      ((= temp 0)
        (princ "Equal points for covering and uncovering.") (terpri)
      )
      ((= temp 1)
        (princ "More points by covering.") (terpri)
      )
      (t
        (princ "More points by uncovering.") (terpri)
      )
    )
  )
)

;********************************************************************* 
;Function Name: findBestCoverMove
;Purpose: To find the a series of numbers to cover using the last uncovered square
;Parameters:
;             roll, an integer. The number rolled
;             board, a list of integers. The board to search through
;             i, an integer. The number of iteration through this function
;Return Value: The best list of squares to cover
;Local Variables:
;                 combination, a list of integers. The best combination of moves
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If i is greater than roll, return an empty list
;           3) If the first element of the board is covered, recurse into this function using the rest of the board and an incremented i
;           4) If the sum of the combination is larger than the roll, return the rest of the combination
;           5) Else, return the combination
;Assistance Received: none 
;*********************************************************************
(defun findBestCoverMove(roll board i)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((> i roll)
          '()
        )
        (t
          (cond
            ((= (first board) 0)
              (findBestCoverMove roll (rest board) (+ i 1))
            )
            (t
              (let* ((combination (cons (first board) (findBestCoverMove roll (rest board) (+ i 1)))))
                (cond
                  ((> (sum combination) roll)
                    (rest combination)
                  )
                  (t
                    combination
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: getBestCoverMove
;Purpose: To find the best series of squares to cover
;Parameters:
;             roll, an integer. The number rolled
;             board, a list of integers. The board to search through
;Return Value: The best list of squares to cover
;Local Variables:
;                 combination, a list of integers. The best combination of moves
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If the sum of the combination is equal to the roll, return the combination
;           5) Else, recurse into this function with the last element of the board removed
;Assistance Received: none 
;*********************************************************************
(defun getBestCoverMove(roll board)
  (cond
    ((null board)
      ()
    )
    (t
      (let* ((combination (findBestCoverMove roll board 1)))
        (cond
          ((= (sum combination) roll)
            combination
          )
          (t
            (getBestCoverMove roll (removeLast board))
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: findBestUncoverMove
;Purpose: To find the a series of numbers to uncover using the last covered square
;Parameters:
;             roll, an integer. The number rolled
;             board, a list of integers. The board to search through
;             i, an integer. The number of iteration through this function
;Return Value: The best list of squares to uncover
;Local Variables:
;                 combination, a list of integers. The best combination of moves
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If i is greater than roll, return an empty list
;           3) If the first element of the board is covered
;             4) If the sum of the combination is larger than the roll, return the rest of the combination
;             5) Else, return the combination
;           6) Else, recurse into this function using the rest of the board and an incremented i
;Assistance Received: none 
;*********************************************************************
(defun findBestUncoverMove(roll board i)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((> i roll)
          '()
        )
        (t
          (cond
            ((= (first board) 0)
              (let* ((combination (cons i (findBestUncoverMove roll (rest board) (+ i 1)))))
                (cond
                  ((> (sum combination) roll)
                    (rest combination)
                  )
                  (t
                    combination
                  )
                )
              )
            )
            (t
              (findBestUncoverMove roll (rest board) (+ i 1))
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: getBestUncoverMove
;Purpose: To find the best series of squares to uncover
;Parameters:
;             roll, an integer. The number rolled
;             board, a list of integers. The board to search through
;Return Value: The best list of squares to uncover
;Local Variables:
;                 combination, a list of integers. The best combination of moves
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If the sum of the combination is equal to the roll, return the combination
;           5) Else, recurse into this function with the last element of the board removed
;Assistance Received: none 
;*********************************************************************
(defun getBestUncoverMove(roll board)
  (cond
    ((null board)
      ()
    )
    (t
      (let* ((combination (findBestUncoverMove roll board 1)))
        (cond
          ((= (sum combination) roll)
            combination
          )
          (t
            (getBestUncoverMove roll (removeLast board))
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: saveGame
;Purpose: To serialize the tournament
;Parameters:
;             humanBoard, a list of integers. The human's board
;             humanScore, an integer. The human's tournament score
;             computerBoard, a list of integers. The computer's board
;             computerScore, an integer. The computer's tournament score
;             firstTurn, a boolean. Represents if the human gets the first turn
;             nextTurn, a boolean. Represents if the human gets the next turn
;Return Value: T
;Local Variables:
;                 stream, output stream to file. Used to write to file
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun saveGame(humanBoard humanScore computerBoard computerScore firstTurn nextTurn)
  (with-open-file (stream "E:/save.txt"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create)
    (format stream "(~%")
    (format stream "  ; Computer:~%")
    (format stream "  (~%")
    (format stream "    ; Squares:~%")
    (format stream "    ") (format stream (format nil "~A" (deformatBoard computerBoard))) (format stream "~%")
    (format stream "    ;Score:~%")
    (format stream "    ") (format stream (format nil "~A" computerScore)) (format stream "~%")
    (format stream "  )~%")
    (format stream "~%")
    (format stream "  ; Human:~%")
    (format stream "  (~%")
    (format stream "    ; Squares:~%")
    (format stream "    ") (format stream (format nil "~A" (deformatBoard humanBoard))) (format stream "~%")
    (format stream "    ; Score:~%")
    (format stream "    ") (format stream (format nil "~A" humanScore)) (format stream "~%")
    (format stream "  )~%")
    (format stream "~%")
    (format stream "  ;First Turn:~%")
    (format stream "  ") (format stream (deformatTurn firstTurn)) (format stream "~%")
    (format stream "~%")
    (format stream "  ;Next Turn:~%")
    (format stream "  ") (format stream (deformatTurn nextTurn)) (format stream "~%")
    (format stream ")~%")
  )
  (excl:exit)
)

;********************************************************************* 
;Function Name: saveRolls
;Purpose: To serialize the dice rolls
;Parameters:
;             rolls, a list of integers. The list to serialize
;Return Value: T
;Local Variables:
;                 stream, output stream to file. Used to write to file
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun saveRolls(rolls)
  (with-open-file (stream "E:/rolls.txt"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create)
    (format stream (format nil "~A" rolls))
  )
)

;********************************************************************* 
;Function Name: readRoll
;Purpose: To generalize a roll
;Parameters: None
;Return Value: T
;Local Variables:
;                 stream, input stream to file. Used to read a file
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun readRoll()
  (with-open-file (stream "E:/rolls.txt"
                  :direction :input
                  :if-exists :supersede
                  :if-does-not-exist :create)
    ;(read-line stream nil)
    (with-input-from-string (s (read-line stream nil) :index i)
      (read s)
    )
  )
)

;********************************************************************* 
;Function Name: promptSave
;Purpose: To prompt the user to save
;Parameters: None
;Return Value: T if yes, NIL if no
;Local Variables:
;                 input, user input integer. Users option
;Algorithm:
;           1) If user selected yes, return T
;           2) If user selected no, return NIL
;           3) Else, recurse into this function
;Assistance Received: none 
;*********************************************************************
(defun promptSave()
  (terpri)
  (princ "Save Game?") (terpri)
  (princ "0) Yes") (terpri)
  (princ "1) No") (terpri)
  (princ "Enter input: ")
  (let* ((input (read)))
    (cond
      ((= input 0)
        t
      )
      ((= input 1)
        nil
      )
      (t
        (princ "Invalid input.") (terpri)
        (promptSave)
      )
    )
  )
)

;********************************************************************* 
;Function Name: loadGame
;Purpose: To generalize a tournament
;Parameters:
;             fileName, a string. The directory to the file
;Return Value: T
;Local Variables:
;                 stream, input stream to file. Used to read a file
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun loadGame(fileName)
  (with-open-file (stream fileName
                  :direction :input
                  :if-exists :supersede)
    (let* ((game '()))
      (read-line stream nil)
      (read-line stream nil)
      (read-line stream nil)
      (read-line stream nil)
      (let* ((computerBoard (formatBoard (with-input-from-string (s (string-trim '(#\Space #\Tab #\Newline) (read-line stream nil)) :index i) (read s) ))))
        (read-line stream nil)
        (let* ((computerScore (with-input-from-string (s (string-trim '(#\Space #\Tab #\Newline) (read-line stream nil)) :index i) (read s) )))
          (read-line stream nil)
          (read-line stream nil)
          (read-line stream nil)
          (read-line stream nil)
          (read-line stream nil)
          (let* ((humanBoard (formatBoard (with-input-from-string (s (string-trim '(#\Space #\Tab #\Newline) (read-line stream nil)) :index i) (read s) ))))
            (read-line stream nil)
            (let* ((humanScore (with-input-from-string (s (string-trim '(#\Space #\Tab #\Newline) (read-line stream nil)) :index i) (read s) )))
              (read-line stream nil)
              (read-line stream nil)
              (read-line stream nil)
              (let* ((firstTurn (formatTurn (with-input-from-string (s (string-trim '(#\Space #\Tab #\Newline) (read-line stream nil)) :index i) (read s) ))))
                (read-line stream nil)
                (read-line stream nil)
                (let* ((nextTurn (formatTurn (with-input-from-string (s (string-trim '(#\Space #\Tab #\Newline) (read-line stream nil)) :index i) (read s) ))))
                  (cons computerBoard (cons computerScore (cons humanBoard (cons humanScore (cons firstTurn (list nextTurn))))))
                )
              )
            )
          )
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: formatBoard
;Purpose: To convert the *s in board to 0s
;Parameters:
;             board, a list of integers and *s. The board to generalize
;Return Value: The board with the *s coverted to 0s
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If the first element in the board is a *, return the rest of the board with a 0 in front
;           3) Else, return the value of the recursed function using the rest of the board with the first of the board in front
;Assistance Received: none 
;*********************************************************************
(defun formatBoard(board)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((eq (first board) '*)
          (cons 0 (formatBoard (rest board)))
        )
        (t
          (cons (first board) (formatBoard (rest board)))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: deformatBoard
;Purpose: To convert the 0s in board to *s
;Parameters:
;             board, a list of integers and *s. The board to serialize
;Return Value: The board with the 0s coverted to *s
;Local Variables: None
;Algorithm:
;           1) If the board is empty, return an empty list
;           2) If the first element in the board is a 0, return the rest of the board with a * in front
;           3) Else, return the value of the recursed function using the rest of the board with the first of the board in front
;Assistance Received: none 
;*********************************************************************
(defun deformatBoard(board)
  (cond
    ((null board)
      '()
    )
    (t
      (cond
        ((= (first board) 0)
          (cons '* (deformatBoard (rest board)))
        )
        (t
          (cons (first board) (deformatBoard (rest board)))
        )
      )
    )
  )
)

;********************************************************************* 
;Function Name: formatTurn
;Purpose: To convert a string to a boolean
;Parameters:
;             word, a string. The word to covert
;Return Value: T if word is HUMAN, NIL otherwise
;Local Variables: None
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun formatTurn(word)
  (cond
    ((eq word 'HUMAN)
      t
    )
    (t
      nil
    )
  )
)

;********************************************************************* 
;Function Name: deformatTurn
;Purpose: To convert a boolean to a string
;Parameters:
;             true_false, a boolean. The boolean to convert
;Return Value: HUMAN if true_false is t, COMPUTER otherwise
;Local Variables: None
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun deformatTurn(true_false)
  (cond
    (true_false
      "HUMAN"
    )
    (t
      "COMPUTER"
    )
  )
)

;********************************************************************* 
;Function Name: removeLast
;Purpose: To remove the last element from a list
;Parameters:
;             board, a list of integers. The list to remove from
;Return Value: The board with the last element removed
;Local Variables: None
;Algorithm:
;           1) If board is empty, return an empty list
;           2) If the rest of the board is empty, return an empty list
;           3) Else, return the recursed value of this function with the rest of the board and the first element add in front
;Assistance Received: none 
;*********************************************************************
(defun removeLast(board)
  (cond
    ((null board)
      ()
    )
    ((null (rest board))
      ()
    )
    (t
      (cons (first board) (removeLast (rest board)))
    )
  )
)

;********************************************************************* 
;Function Name: main
;Purpose: To run the progam
;Parameters: None
;Return Value: T
;Local Variables: None
;Algorithm: None
;Assistance Received: none 
;*********************************************************************
(defun main()
  (mainMenu) (terpri)
)

;********************************************************************* 
;Starts the program upon load.
;*********************************************************************
(main)
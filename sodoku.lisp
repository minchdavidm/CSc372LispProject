#|
--   Assignment:  Project Part #3:  Sodoku Solver
--       Author:  David Minch (minchdavidm@email.arizona.edu)
--
--       Course:  CSc 372
--   Instructor:  L. McCann
--           TA:  Tito Ferra
--     Due Date:  December 9th, 2019
--
--  Description:  This program accepts one or two values from standard input, a filename
--                enclosed in quotes. It will then read the file located at ./filename,
--                interpret its first 81 digits in [1-9] as values of a sodoku, and
--                solve the resultant Sodoku. There will be two output files: ./filename.solution,
--                which will contain the solved sodoku, and optionally ./filename.log, which
--                contains a log of everything the solver tries to solve the sodoku if a second
--                value form standard input is given..
--
-- Execution Ex:  The following example will read the file input.txt and
--                create a file input.txt.sodoku containing the solved sodoku
--                of input.txt. Note the quotes around the filename.
--                $ clisp sodoku.lisp
--                "input.txt"
--
-- Execution Ex:  The following example will read the file input.txt and
--                create a file input.txt.sodoku containing the solved sodoku
--                of input.txt, and input.txt.log containing the steps taken to solve
--                the sodoku.
--                $ echo "\"input.txt\" x"
--
--     Language:  Lisp (Common Lisp)
-- Ex. Packages:  None.
--
--        Notes:  The program waits for the filename, but if the second value is not
--                provided immediately, the program will close STDIN and start solving
--                the sodoku.
--
-- Deficiencies: None.
--
|#

#|
  cell struct
  A cell contains a pointer to the row, column and box in which it is located.
  It also contains a list of potential values the cell might hold, and the value
  it does hold (initially '_').
|#
(defstruct cell
  row ; List of 9 cells
  col ; List of 9 cells
  box ; List of 9 cells
  potential ; List of 9 characters 1,2,3,4,5,6,7,8 and 9
  value ; A single character _,1,2,3,4,5,6,7,8, or 9
  flag ; int 0 if the cell hasn't been checked, 1 if it has been checked.
)

#|
  solve sodoku function
  purpose: This function solves a given sodoku board, or returns null if it's a bad board.
           Because of this structre, if the algorithm ever needs to guess, then it recursively
           calls solve sodoku on the sodoku with the guess in it.
  pre-conditions: sodoku is a list of cells
  post-conditions: This is currently not a pure function, it alters the sodoku passed in. If I have time to improve it,
                   then I'll make it copy the sodoku and solve it, returning a new sodoku and leaving the
                   original alone, thus avoiding side effects.
                   Also, if logfile is not null, the steps taken by this algorithm will be printed to it.
  returns: The solved sodoku board or an empty list (a null) if it's a failure
  parameters: sodoku, the board as a list of 81 cells, and logfile, the string for where we log to.
|#
(defun solve-sodoku (sodoku logfile)
  (log-sodoku logfile "BEGIN: attempt to solve the following sodoku:" sodoku)




  sodoku
)

#|
  find next sodoku function
  purpose: This function looks at the sodoku board and, starting from the upper left corner and uses
           reduction tactics to find a cell that can be filled with a value.
  pre-conditions: The flags of the sodoku are 0 if they need to be checked, 1s can be skipped over
  post-conditions: None, nothing is modified by this function
  returns: A 2 element list, holding (value index). If value is 0, it's because we didn't find one, so we need to guess.
  parameters: sodoku, the list of 81 cells, and logfile, where we write things to.
  Reduction Tactics currently in use:
    -- None, always suggests guessing.
|#
(defun find-next-sodoku (sodoku logfile)
  (setq found (list #\0 0))
  (log-sodoku logfile "Unable to deduce another solution in the sodoku:" sodoku)
  found
)

#|
  build sodoku function
  purpose: This function builds a blank sodoku board, constructing all the row, col and box pointers
  pre-conditions: None
  post-conditions: None, nothing is modified by this function
  returns: A sodoku board : The returned struct will be a list of 81 cells, each will potential list 1-9 and value _
  parameters: None
|#

(defun build-sodoku ()
  (setq sodoku (list)) ; empty list, we'll be appending to it
  (setq i 0) ; iterator
  ; First, create all the cells
  (loop
    (when (= i 81) (return))

    (setq cell (make-cell
      :potential (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      :value #\_
      :flag 0
      )
    )
    ; add this new cell to the sodoku.
    (setq sodoku (append sodoku (list cell)))
    (setq i (+ i 1))
  )
  ; Build the rows. For row n, the values are n*9 -> n*9 + 8

  (setq row0 (list (nth 0 sodoku) (nth 1 sodoku) (nth 2 sodoku) (nth 3 sodoku) (nth 4 sodoku) (nth 5 sodoku) (nth 6 sodoku) (nth 7 sodoku) (nth 8 sodoku)))
  (setf (cell-row (nth 0 sodoku)) row0)  (setf (cell-row (nth 1 sodoku)) row0)  (setf (cell-row (nth 2 sodoku)) row0)
  (setf (cell-row (nth 3 sodoku)) row0)  (setf (cell-row (nth 4 sodoku)) row0)  (setf (cell-row (nth 5 sodoku)) row0)
  (setf (cell-row (nth 6 sodoku)) row0)  (setf (cell-row (nth 7 sodoku)) row0)  (setf (cell-row (nth 8 sodoku)) row0)

  (setq row1 (list (nth 9 sodoku) (nth 10 sodoku) (nth 11 sodoku) (nth 12 sodoku) (nth 13 sodoku) (nth 14 sodoku) (nth 15 sodoku) (nth 16 sodoku) (nth 17 sodoku)))
  (setf (cell-row (nth 9 sodoku)) row1)  (setf (cell-row (nth 10 sodoku)) row1) (setf (cell-row (nth 11 sodoku)) row1)
  (setf (cell-row (nth 12 sodoku)) row1) (setf (cell-row (nth 13 sodoku)) row1) (setf (cell-row (nth 14 sodoku)) row1)
  (setf (cell-row (nth 15 sodoku)) row1) (setf (cell-row (nth 16 sodoku)) row1) (setf (cell-row (nth 17 sodoku)) row1)

  (setq row2 (list (nth 18 sodoku) (nth 19 sodoku) (nth 20 sodoku) (nth 21 sodoku) (nth 22 sodoku) (nth 23 sodoku) (nth 24 sodoku) (nth 25 sodoku) (nth 26 sodoku)))
  (setf (cell-row (nth 18 sodoku)) row2) (setf (cell-row (nth 19 sodoku)) row2) (setf (cell-row (nth 20 sodoku)) row2)
  (setf (cell-row (nth 21 sodoku)) row2) (setf (cell-row (nth 22 sodoku)) row2) (setf (cell-row (nth 23 sodoku)) row2)
  (setf (cell-row (nth 24 sodoku)) row2) (setf (cell-row (nth 25 sodoku)) row2) (setf (cell-row (nth 26 sodoku)) row2)

  (setq row3 (list (nth 27 sodoku) (nth 28 sodoku) (nth 29 sodoku) (nth 30 sodoku) (nth 31 sodoku) (nth 32 sodoku) (nth 33 sodoku) (nth 34 sodoku) (nth 35 sodoku)))
  (setf (cell-row (nth 27 sodoku)) row3) (setf (cell-row (nth 28 sodoku)) row3) (setf (cell-row (nth 29 sodoku)) row3)
  (setf (cell-row (nth 30 sodoku)) row3) (setf (cell-row (nth 31 sodoku)) row3) (setf (cell-row (nth 32 sodoku)) row3)
  (setf (cell-row (nth 33 sodoku)) row3) (setf (cell-row (nth 34 sodoku)) row3) (setf (cell-row (nth 35 sodoku)) row3)

  (setq row4 (list (nth 36 sodoku) (nth 37 sodoku) (nth 38 sodoku) (nth 39 sodoku) (nth 40 sodoku) (nth 41 sodoku) (nth 42 sodoku) (nth 43 sodoku) (nth 44 sodoku)))
  (setf (cell-row (nth 36 sodoku)) row4) (setf (cell-row (nth 37 sodoku)) row4) (setf (cell-row (nth 38 sodoku)) row4)
  (setf (cell-row (nth 39 sodoku)) row4) (setf (cell-row (nth 40 sodoku)) row4) (setf (cell-row (nth 41 sodoku)) row4)
  (setf (cell-row (nth 42 sodoku)) row4) (setf (cell-row (nth 43 sodoku)) row4) (setf (cell-row (nth 44 sodoku)) row4)

  (setq row5 (list (nth 45 sodoku) (nth 46 sodoku) (nth 47 sodoku) (nth 48 sodoku) (nth 49 sodoku) (nth 50 sodoku) (nth 51 sodoku) (nth 52 sodoku) (nth 53 sodoku)))
  (setf (cell-row (nth 45 sodoku)) row5) (setf (cell-row (nth 46 sodoku)) row5) (setf (cell-row (nth 47 sodoku)) row5)
  (setf (cell-row (nth 48 sodoku)) row5) (setf (cell-row (nth 49 sodoku)) row5) (setf (cell-row (nth 50 sodoku)) row5)
  (setf (cell-row (nth 51 sodoku)) row5) (setf (cell-row (nth 52 sodoku)) row5) (setf (cell-row (nth 53 sodoku)) row5)

  (setq row6 (list (nth 54 sodoku) (nth 55 sodoku) (nth 56 sodoku) (nth 57 sodoku) (nth 58 sodoku) (nth 59 sodoku) (nth 60 sodoku) (nth 61 sodoku) (nth 62 sodoku)))
  (setf (cell-row (nth 54 sodoku)) row6) (setf (cell-row (nth 55 sodoku)) row6) (setf (cell-row (nth 56 sodoku)) row6)
  (setf (cell-row (nth 57 sodoku)) row6) (setf (cell-row (nth 58 sodoku)) row6) (setf (cell-row (nth 59 sodoku)) row6)
  (setf (cell-row (nth 60 sodoku)) row6) (setf (cell-row (nth 61 sodoku)) row6) (setf (cell-row (nth 62 sodoku)) row6)

  (setq row7 (list (nth 63 sodoku) (nth 64 sodoku) (nth 65 sodoku) (nth 66 sodoku) (nth 67 sodoku) (nth 68 sodoku) (nth 69 sodoku) (nth 70 sodoku) (nth 71 sodoku)))
  (setf (cell-row (nth 63 sodoku)) row7) (setf (cell-row (nth 64 sodoku)) row7) (setf (cell-row (nth 65 sodoku)) row7)
  (setf (cell-row (nth 66 sodoku)) row7) (setf (cell-row (nth 67 sodoku)) row7) (setf (cell-row (nth 68 sodoku)) row7)
  (setf (cell-row (nth 69 sodoku)) row7) (setf (cell-row (nth 70 sodoku)) row7) (setf (cell-row (nth 71 sodoku)) row7)

  (setq row8 (list (nth 72 sodoku) (nth 73 sodoku) (nth 74 sodoku) (nth 75 sodoku) (nth 76 sodoku) (nth 77 sodoku) (nth 78 sodoku) (nth 79 sodoku) (nth 80 sodoku)))
  (setf (cell-row (nth 72 sodoku)) row8) (setf (cell-row (nth 73 sodoku)) row8) (setf (cell-row (nth 74 sodoku)) row8)
  (setf (cell-row (nth 75 sodoku)) row8) (setf (cell-row (nth 76 sodoku)) row8) (setf (cell-row (nth 77 sodoku)) row8)
  (setf (cell-row (nth 78 sodoku)) row8) (setf (cell-row (nth 79 sodoku)) row8) (setf (cell-row (nth 80 sodoku)) row8)

  ; build the columns. For column n, our cells are located at n + 9x, where x \in 0-8

  (setq col0 (list (nth 0 sodoku) (nth 9 sodoku) (nth 18 sodoku) (nth 27 sodoku) (nth 36 sodoku) (nth 45 sodoku) (nth 54 sodoku) (nth 63 sodoku) (nth 72 sodoku)))
  (setf (cell-col (nth 0 sodoku)) col0)  (setf (cell-col (nth 9 sodoku)) col0)  (setf (cell-col (nth 18 sodoku)) col0)
  (setf (cell-col (nth 27 sodoku)) col0) (setf (cell-col (nth 36 sodoku)) col0) (setf (cell-col (nth 45 sodoku)) col0)
  (setf (cell-col (nth 54 sodoku)) col0) (setf (cell-col (nth 63 sodoku)) col0) (setf (cell-col (nth 72 sodoku)) col0)

  (setq col1 (list (nth 1 sodoku) (nth 10 sodoku) (nth 19 sodoku) (nth 28 sodoku) (nth 37 sodoku) (nth 46 sodoku) (nth 55 sodoku) (nth 64 sodoku) (nth 73 sodoku)))
  (setf (cell-col (nth 1 sodoku)) col1)  (setf (cell-col (nth 10 sodoku)) col1) (setf (cell-col (nth 19 sodoku)) col1)
  (setf (cell-col (nth 28 sodoku)) col1) (setf (cell-col (nth 37 sodoku)) col1) (setf (cell-col (nth 46 sodoku)) col1)
  (setf (cell-col (nth 55 sodoku)) col1) (setf (cell-col (nth 64 sodoku)) col1) (setf (cell-col (nth 73 sodoku)) col1)

  (setq col2 (list (nth 2 sodoku) (nth 11 sodoku) (nth 20 sodoku) (nth 29 sodoku) (nth 38 sodoku) (nth 47 sodoku) (nth 56 sodoku) (nth 65 sodoku) (nth 74 sodoku)))
  (setf (cell-col (nth 2 sodoku)) col2)  (setf (cell-col (nth 11 sodoku)) col2) (setf (cell-col (nth 20 sodoku)) col2)
  (setf (cell-col (nth 29 sodoku)) col2) (setf (cell-col (nth 38 sodoku)) col2) (setf (cell-col (nth 47 sodoku)) col2)
  (setf (cell-col (nth 56 sodoku)) col2) (setf (cell-col (nth 65 sodoku)) col2) (setf (cell-col (nth 74 sodoku)) col2)

  (setq col3 (list (nth 3 sodoku) (nth 12 sodoku) (nth 21 sodoku) (nth 30 sodoku) (nth 39 sodoku) (nth 48 sodoku) (nth 57 sodoku) (nth 66 sodoku) (nth 75 sodoku)))
  (setf (cell-col (nth 3 sodoku)) col3)  (setf (cell-col (nth 12 sodoku)) col3) (setf (cell-col (nth 21 sodoku)) col3)
  (setf (cell-col (nth 30 sodoku)) col3) (setf (cell-col (nth 39 sodoku)) col3) (setf (cell-col (nth 48 sodoku)) col3)
  (setf (cell-col (nth 57 sodoku)) col3) (setf (cell-col (nth 66 sodoku)) col3) (setf (cell-col (nth 75 sodoku)) col3)

  (setq col4 (list (nth 4 sodoku) (nth 13 sodoku) (nth 22 sodoku) (nth 31 sodoku) (nth 40 sodoku) (nth 49 sodoku) (nth 58 sodoku) (nth 67 sodoku) (nth 76 sodoku)))
  (setf (cell-col (nth 4 sodoku)) col4)  (setf (cell-col (nth 13 sodoku)) col4) (setf (cell-col (nth 22 sodoku)) col4)
  (setf (cell-col (nth 31 sodoku)) col4) (setf (cell-col (nth 40 sodoku)) col4) (setf (cell-col (nth 49 sodoku)) col4)
  (setf (cell-col (nth 58 sodoku)) col4) (setf (cell-col (nth 67 sodoku)) col4) (setf (cell-col (nth 76 sodoku)) col4)

  (setq col5 (list (nth 5 sodoku) (nth 14 sodoku) (nth 23 sodoku) (nth 32 sodoku) (nth 41 sodoku) (nth 50 sodoku) (nth 59 sodoku) (nth 68 sodoku) (nth 77 sodoku)))
  (setf (cell-col (nth 5 sodoku)) col5)  (setf (cell-col (nth 14 sodoku)) col5) (setf (cell-col (nth 23 sodoku)) col5)
  (setf (cell-col (nth 32 sodoku)) col5) (setf (cell-col (nth 41 sodoku)) col5) (setf (cell-col (nth 50 sodoku)) col5)
  (setf (cell-col (nth 59 sodoku)) col5) (setf (cell-col (nth 68 sodoku)) col5) (setf (cell-col (nth 77 sodoku)) col5)

  (setq col6 (list (nth 6 sodoku) (nth 15 sodoku) (nth 24 sodoku) (nth 33 sodoku) (nth 42 sodoku) (nth 51 sodoku) (nth 60 sodoku) (nth 69 sodoku) (nth 78 sodoku)))
  (setf (cell-col (nth 6 sodoku)) col6)  (setf (cell-col (nth 15 sodoku)) col6) (setf (cell-col (nth 24 sodoku)) col6)
  (setf (cell-col (nth 33 sodoku)) col6) (setf (cell-col (nth 42 sodoku)) col6) (setf (cell-col (nth 51 sodoku)) col6)
  (setf (cell-col (nth 60 sodoku)) col6) (setf (cell-col (nth 69 sodoku)) col6) (setf (cell-col (nth 78 sodoku)) col6)

  (setq col7 (list (nth 7 sodoku) (nth 16 sodoku) (nth 25 sodoku) (nth 34 sodoku) (nth 43 sodoku) (nth 52 sodoku) (nth 61 sodoku) (nth 70 sodoku) (nth 79 sodoku)))
  (setf (cell-col (nth 7 sodoku)) col7)  (setf (cell-col (nth 16 sodoku)) col7) (setf (cell-col (nth 25 sodoku)) col7)
  (setf (cell-col (nth 34 sodoku)) col7) (setf (cell-col (nth 43 sodoku)) col7) (setf (cell-col (nth 52 sodoku)) col7)
  (setf (cell-col (nth 61 sodoku)) col7) (setf (cell-col (nth 70 sodoku)) col7) (setf (cell-col (nth 79 sodoku)) col7)

  (setq col8 (list (nth 8 sodoku) (nth 17 sodoku) (nth 26 sodoku) (nth 35 sodoku) (nth 44 sodoku) (nth 53 sodoku) (nth 62 sodoku) (nth 71 sodoku) (nth 80 sodoku)))
  (setf (cell-col (nth 8 sodoku)) col8)  (setf (cell-col (nth 17 sodoku)) col8) (setf (cell-col (nth 26 sodoku)) col8)
  (setf (cell-col (nth 35 sodoku)) col8) (setf (cell-col (nth 44 sodoku)) col8) (setf (cell-col (nth 53 sodoku)) col8)
  (setf (cell-col (nth 62 sodoku)) col8) (setf (cell-col (nth 71 sodoku)) col8) (setf (cell-col (nth 80 sodoku)) col8)

  ; build the boxes. For box n, our cells are located at n+1, n+2, n+3, n+9, n+10, n+11, n+18, n+19 and n+20

  (setq box0 (list (nth 0 sodoku) (nth 1 sodoku) (nth 2 sodoku) (nth 9 sodoku) (nth 10 sodoku) (nth 11 sodoku) (nth 18 sodoku) (nth 19 sodoku) (nth 20 sodoku)))
  (setf (cell-box (nth 0 sodoku)) box0)  (setf (cell-box (nth 1 sodoku)) box0)  (setf (cell-box (nth 2 sodoku)) box0)
  (setf (cell-box (nth 9 sodoku)) box0)  (setf (cell-box (nth 10 sodoku)) box0) (setf (cell-box (nth 11 sodoku)) box0)
  (setf (cell-box (nth 18 sodoku)) box0) (setf (cell-box (nth 19 sodoku)) box0) (setf (cell-box (nth 20 sodoku)) box0)

  (setq box1 (list (nth 3 sodoku) (nth 4 sodoku) (nth 5 sodoku) (nth 12 sodoku) (nth 13 sodoku) (nth 14 sodoku) (nth 21 sodoku) (nth 22 sodoku) (nth 23 sodoku)))
  (setf (cell-box (nth 3 sodoku)) box1)  (setf (cell-box (nth 4 sodoku)) box1)  (setf (cell-box (nth 5 sodoku)) box1)
  (setf (cell-box (nth 12 sodoku)) box1) (setf (cell-box (nth 13 sodoku)) box1) (setf (cell-box (nth 14 sodoku)) box1)
  (setf (cell-box (nth 21 sodoku)) box1) (setf (cell-box (nth 22 sodoku)) box1) (setf (cell-box (nth 23 sodoku)) box1)

  (setq box2 (list (nth 6 sodoku) (nth 7 sodoku) (nth 8 sodoku) (nth 15 sodoku) (nth 16 sodoku) (nth 17 sodoku) (nth 24 sodoku) (nth 25 sodoku) (nth 26 sodoku)))
  (setf (cell-box (nth 6 sodoku)) box2)  (setf (cell-box (nth 7 sodoku)) box2)  (setf (cell-box (nth 8 sodoku)) box2)
  (setf (cell-box (nth 15 sodoku)) box2) (setf (cell-box (nth 16 sodoku)) box2) (setf (cell-box (nth 17 sodoku)) box2)
  (setf (cell-box (nth 24 sodoku)) box2) (setf (cell-box (nth 25 sodoku)) box2) (setf (cell-box (nth 26 sodoku)) box2)

  (setq box3 (list (nth 27 sodoku) (nth 28 sodoku) (nth 29 sodoku) (nth 36 sodoku) (nth 37 sodoku) (nth 38 sodoku) (nth 45 sodoku) (nth 46 sodoku) (nth 47 sodoku)))
  (setf (cell-box (nth 27 sodoku)) box3) (setf (cell-box (nth 28 sodoku)) box3) (setf (cell-box (nth 29 sodoku)) box3)
  (setf (cell-box (nth 36 sodoku)) box3) (setf (cell-box (nth 37 sodoku)) box3) (setf (cell-box (nth 38 sodoku)) box3)
  (setf (cell-box (nth 45 sodoku)) box3) (setf (cell-box (nth 46 sodoku)) box3) (setf (cell-box (nth 47 sodoku)) box3)

  (setq box4 (list (nth 30 sodoku) (nth 31 sodoku) (nth 32 sodoku) (nth 39 sodoku) (nth 40 sodoku) (nth 41 sodoku) (nth 48 sodoku) (nth 49 sodoku) (nth 50 sodoku)))
  (setf (cell-box (nth 30 sodoku)) box4) (setf (cell-box (nth 31 sodoku)) box4) (setf (cell-box (nth 32 sodoku)) box4)
  (setf (cell-box (nth 39 sodoku)) box4) (setf (cell-box (nth 40 sodoku)) box4) (setf (cell-box (nth 41 sodoku)) box4)
  (setf (cell-box (nth 48 sodoku)) box4) (setf (cell-box (nth 49 sodoku)) box4) (setf (cell-box (nth 50 sodoku)) box4)

  (setq box5 (list (nth 33 sodoku) (nth 34 sodoku) (nth 35 sodoku) (nth 42 sodoku) (nth 43 sodoku) (nth 44 sodoku) (nth 51 sodoku) (nth 52 sodoku) (nth 53 sodoku)))
  (setf (cell-box (nth 33 sodoku)) box5) (setf (cell-box (nth 34 sodoku)) box5) (setf (cell-box (nth 35 sodoku)) box5)
  (setf (cell-box (nth 42 sodoku)) box5) (setf (cell-box (nth 43 sodoku)) box5) (setf (cell-box (nth 44 sodoku)) box5)
  (setf (cell-box (nth 51 sodoku)) box5) (setf (cell-box (nth 52 sodoku)) box5) (setf (cell-box (nth 53 sodoku)) box5)

  (setq box6 (list (nth 54 sodoku) (nth 55 sodoku) (nth 56 sodoku) (nth 63 sodoku) (nth 64 sodoku) (nth 65 sodoku) (nth 72 sodoku) (nth 73 sodoku) (nth 74 sodoku)))
  (setf (cell-box (nth 54 sodoku)) box6) (setf (cell-box (nth 55 sodoku)) box6) (setf (cell-box (nth 56 sodoku)) box6)
  (setf (cell-box (nth 63 sodoku)) box6) (setf (cell-box (nth 64 sodoku)) box6) (setf (cell-box (nth 65 sodoku)) box6)
  (setf (cell-box (nth 72 sodoku)) box6) (setf (cell-box (nth 73 sodoku)) box6) (setf (cell-box (nth 74 sodoku)) box6)

  (setq box7 (list (nth 57 sodoku) (nth 58 sodoku) (nth 59 sodoku) (nth 66 sodoku) (nth 67 sodoku) (nth 68 sodoku) (nth 75 sodoku) (nth 76 sodoku) (nth 77 sodoku)))
  (setf (cell-box (nth 57 sodoku)) box7) (setf (cell-box (nth 58 sodoku)) box7) (setf (cell-box (nth 59 sodoku)) box7)
  (setf (cell-box (nth 66 sodoku)) box7) (setf (cell-box (nth 67 sodoku)) box7) (setf (cell-box (nth 68 sodoku)) box7)
  (setf (cell-box (nth 75 sodoku)) box7) (setf (cell-box (nth 76 sodoku)) box7) (setf (cell-box (nth 77 sodoku)) box7)

  (setq box8 (list (nth 60 sodoku) (nth 61 sodoku) (nth 62 sodoku) (nth 69 sodoku) (nth 70 sodoku) (nth 71 sodoku) (nth 78 sodoku) (nth 79 sodoku) (nth 80 sodoku)))
  (setf (cell-box (nth 60 sodoku)) box8) (setf (cell-box (nth 61 sodoku)) box8) (setf (cell-box (nth 62 sodoku)) box8)
  (setf (cell-box (nth 69 sodoku)) box8) (setf (cell-box (nth 70 sodoku)) box8) (setf (cell-box (nth 71 sodoku)) box8)
  (setf (cell-box (nth 78 sodoku)) box8) (setf (cell-box (nth 79 sodoku)) box8) (setf (cell-box (nth 80 sodoku)) box8)

  sodoku ; return the finished blank sodoku
)

#| write-sodoku function
   purpose: This function writes the sodoku to the output file
   pre-conditions: sodoku is a list of cells
   post-conditions: The file filename contains the sodoku
   returns: nothing
   parameters: sodoku, a list of 81 cells
               filename, the string to write to
|#
(defun write-sodoku(sodoku filename)
  (with-open-file (outputStream filename :direction :output)
    (when (/= 0 (length sodoku))
      (setq i 0) ; count so that we can put newlines in the right place
      (dolist (cell sodoku)
        (write-char (cell-value cell) outputStream)
        (if (= (mod i 9) 8) (write-char #\Linefeed outputStream) (write-char #\SPACE outputStream))
        (setq i (+ i 1))
      )
    )
    (when (=  0 (length sodoku))
      (format outputStream "Failed to process sodoku, no solution exists.")
      (terpri outputStream)
    )
  )
)

#| read-sodoku function
   purpose: This function reads a sodoku from a file, which
            is interpreted as the first 81 digits in the file.
            0s and _ are both intepreted as blanks so that it can handle
            more sodokus.
  pre-conditions: sodoku is a list of cells, filename contains the sodoku
  post-conditions: the returned sodoku will be populated with the information from filename
  returns: sodoku, the new sodoku
  parameters: sodoku, a list of 81 cells
              filename, the string to write from
|#
(defun read-sodoku(sodoku filename)
  (with-open-file (inputStream filename :direction :input)
    (setq i 0) ; count to place things in the right place
    (loop
      ; Stop reading if we fill our sodoku
      (when (> i 80) (return))
      ; Also stop reading if we hit end of file. Set i = 82 so that we know we failed to fill a sodoku
      (when (string= (write-to-string (peek-char nil inputStream nil)) (write-to-string nil))
        (setq i 82)
        (return)
      )
      ; Get the character from the file. If it's a digit or _, then add it and increment i. Otherwise, continue
      (setq c (read-char inputStream))
      ; Translate _ to 0 so that we can simply compare 0<=x<=9
      (when (char= #\_ c) (setq c #\0))
      (when (and (char>= c #\0) (char<= c #\9))
        ; Translate 0 to _; this gets all 0s and _ to the same character _.
        (when (char= #\0 c) (setq c #\_))
        (setf (cell-value (nth i sodoku)) c)
        (setf (cell-potential (nth i sodoku)) () ) ; set potential list to null, simplifies stuff later when we check lengths
        (setq i (+ i 1))
      )
    )
  )
  sodoku
)

#| Log function
   purpose: This function logs a string to the logfile, followed by a copy of the
            current sodoku state.
   pre-conditions: If logfile is null, this function will do nothing. Otherwise, the string and sodoku must be populated
   post-conditions: If logfile is not null, the file named the same will contain the string and sodoku
   returns: None
   parameters: logfile, the file to write to (or null),
               text, the string to write, and
               sodoku, the sodoku to write after the text.
|#
(defun log-sodoku(logfile text sodoku)
  (when (/= 0 (length logfile))
    (with-open-file (outputStream logfile :direction :output :if-exists :append :if-does-not-exist :create)
      (format outputStream text)
      (terpri outputStream)
      (terpri outputStream)
      ; Now output the sodoku
      (setq i 0) ; count so that we can put newlines in the right place
      (dolist (cell sodoku)
        (write-char (cell-value cell) outputStream)
        (if (= (mod i 9) 8) (write-char #\Linefeed outputStream) (write-char #\SPACE outputStream))
        (setq i (+ i 1))
      )
      (terpri outputStream)
    ) ; close logfile
  ) ; end when
)


; main routine. Get the filename from the user, check if we are logging,
; then build the sodoku board, read in the initial values, and solve it.

; Get the filename from the user
(setq filename (read))
(unless (stringp filename)
  (setq filename (write-to-string filename))
)
(setq outfile (concatenate 'string filename ".solution"))

; Check if the log flag is given
(setq logfile ())
(if (listen) (setq logfile (concatenate 'string filename ".log")))

; Get empty sodoku board
(setq sodokuBoard (build-sodoku))

; read in sodoku
(setq sodoku (read-sodoku sodokuBoard filename))

; solve the sodoku
(setq sodoku (solve-sodoku sodoku logfile))

; print finished sodoku to file or report error
(write-sodoku sodoku outfile)
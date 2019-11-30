#|
--   Assignment:  Project Part #2:  Rot Exercise
--       Author:  David Minch (minchdavidm@email.arizona.edu)
--
--       Course:  CSc 372
--   Instructor:  L. McCann
--           TA:  Tito Ferra
--     Due Date:  November 25th, 2019
--
--  Description:  This program accepts two values from standard input, a filename
--                enclosed in quotes and an integer, not enclosed in quotes.
--                It will then read the file located at ./filename,
--                and output each character of ./filename rotated by the integer
--                number of characters to a new file named filename.rotN where
--                N is the number given as the second element of stdin..
--
-- Execution Ex:  The following example will read the file input.txt and
--                create a file input.txt.rot-3 containing the contents
--                of input.txt rotated left 3 characters. Note the quotes
--                around the filename.
--                $ clisp rotn.lisp
--                "input.txt"
--                -3
--
--     Language:  Lisp (Common Lisp)
-- Ex. Packages:  None.
--
--        Notes:  If the first value is given without quotes, lisp will
--                read it as a symbol object, which ignores case. I decided
--                to support this and print a warning instead of simply
--                failing, since on Windows filenames aren't case 
--                sensitive so it doesn't make a difference for input.
--                However, on lectura it will fail, so it prints a warning
--                letting the user know why if applicable.
-- 
-- Deficiencies: None.
--
-- Language Study: https://docs.google.com/document/d/13o5NmuXSi3TCT63zvpnYEQUfa2nuQyQkwXJ99w_1_3M/edit?usp=sharing
--
|#

#| get-output-filename function
   purpose: This function takes a filename and integer and builds the
            output filename from it, which is FILENAME.rotN where
            n is the integer.
   pre-conditions: filename is a String, N can be any value but should be integer
   post-conditions: None, this function doesn't modify anything.
   returns: outfilename, as described in the purpose.
   parameters: filename, a string filename
               n       , an integer
|#
(defun get-output-filename(filename n)
  (concatenate 'string filename ".rot" (write-to-string n)))

#| rotate function
   purpose: This function takes an input and output filename and integer, 
            reads the characters from the input filename, if they're a letter it
            rotates them by n characters, and then outputs them to the outfilename.
   pre-conditions: inputFilename is a string representing a real file, 
                   outputFilename can be written to,
                   n should be an integer.
   post-conditions: The file with name outputFilename contians the rotated text of inputFilename
   returns: nothing
   parameters: inputFilename , a string filename of a real text file
               outputFilename, a string filename to write to
               n             , an integer
|#
(defun rotate (inputFilename outputFilename n)
  (with-open-file (inputStream inputFilename :direction :input)
    (with-open-file (outputStream outputFilename :direction :output)
      (loop
        ; return when we hit end of file
        ; Note that this works because write-to-string returns a single character
        ; when we're reading the file and nil when it doesn't, and the string
        ; representation of nil is more than one character, so no character
        ; that can be in the file will ever match this string=. 
        (when (string= (write-to-string (peek-char nil inputStream nil)) (write-to-string nil)) (return))
        ; If we didn't return, write the character to the output stream.
        (write-char (rotate-char (read-char inputStream) n) outputStream))
      )
    )
  n)

#| rotate function
   purpose: This function takes a character and rotates it by n if it's a letter,
            or returns it as is if it is not.
   pre-conditions: c is not nil
                   n is an integer.
   post-conditions: None, nothing is modified by this function
   returns: retchar, which is c rotated by n
   parameters: c, a character to rotate by n
               n, an integer
|# 
(defun rotate-char (c n)
  (setq retchar c)
  ; check if letter. If it is, rotate it
  ; lowercase case
  (when (and (char>= c #\a) (char<= c #\z)) 
    (setq arr #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
    ; get number a=0, z=25 of this character
    (setq numchar (position c arr))
    ; add n to that number, mod 26, to get our rotated character
    (setq numchar (mod (+ numchar n) 26))
    ; Now fetch the rotated character number from the array to get the character
    (setq retchar (aref arr numchar))
  )
  ; uppercase case
  (when (and (char>= c #\A) (char<= c #\Z))
    (setq arr #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
    ; get number A=0, Z=25 of this character
    (setq numchar (position c arr))
    ; add n to that number, mod 26, to get our rotated character
    (setq numchar (mod (+ numchar n) 26))
    ; Now fetch the rotated character number from the array to get the character
    (setq retchar (aref arr numchar))
  )
  ; This now is rotated if it was a letter, return it.
  retchar)

; Main routine. Get the filename and n from user, deduce the output filename,
; then rotate the characters of the filename and print them to the output file
(setq filename (read))
(setq n        (read))
; Warn the user if they gave a symbol instead of a string. Still do the operation though.
(when (symbolp filename)
  (print "Warning: Filenames should be enclosed in quotes")
  (print "         Otherwise, case gets ignored.")
)
(unless (stringp filename)
  (setq filename (write-to-string filename)) 
)
(setq outfilename (get-output-filename filename n ))
(rotate filename outfilename n)

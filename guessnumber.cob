            >>SOURCE FORMAT IS FREE
            *> The above, situated at Line 1, Column 12, allows us to code
            *> without having to reference column numbers. Old COBOLers will
            *> understand and be grateful for this feature in GnuCOBOL.

*> ***************************************************************
*> Guess my number game
*>
*> --A COBOL version of a silly game I made on my programmable
*> calculator when I was bored in math class in 1987, with a couple of
*> additions like input validation and computer guesses.
*>
*> Copyright (c) 2007 Matthew Helmke for the old Python 2 version
*> Copyright (c) 2021 Matthew Helmke for the COBOL version (this one)
*>
*> To compile (Linux with GnuCOBOL):
*>    cobc -x guessnumber.cob
*> To run:
*>    ./guessnumber
*>
*> This program is free software; you can redistribute it and\or
*> modify it under the terms of the GNU General Public License
*> as published by the Free Software Foundation; either version 2
*> of the License, or (at your option) any later version.
*>
*> This program is distributed in the hope that it will be useful,
*> but WITHOUT ANY WARRANTY; without even the implied warranty of
*> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*> GNU General Public License for more details.
*>
*> You should have received a copy of the GNU General Public License
*> along with this program; if not, write to the Free Software
*> Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*> ***************************************************************

*> Throughout, TK means "to come". This program is unfinished as long as
*> this note remains.



 IDENTIFICATION DIVISION.
 PROGRAM-ID. GuessMyNumber.

*> ***************************************************************

 DATA DIVISION.
 WORKING-STORAGE SECTION.
    *> Define variables
    01 USERGUESS PIC 999.
    01 SECRETNUMBER PIC 999.
    01 TOTALGUESSES PIC 99.
    01 LOWMAX PIC 99.
    01 HIGHMAX PIC 999.
    *> the following is for random number generation needs and comes from
    *> an example in https://gnucobol.sourceforge.io/faq/index.html#function-random
    01 Pseudo-Random-Number usage comp-1.


*> ***************************************************************

 PROCEDURE DIVISION.
    *> Assign values to some variables
    COMPUTE TOTALGUESSES = 0.
    COMPUTE LOWMAX = 0.
    COMPUTE HIGHMAX = 100.

    *> FUNCTION RANDOM is pseudo-random, not true random, but good enough
    *> for this game. It will, however, always select the same set of numbers
    *> unless I come back and change how the function is seeded.
    *> https://gnucobol.sourceforge.io/faq/index.html#function-random
    ComputeSecretNumber.
      MOVE FUNCTION RANDOM    TO Pseudo-Random-Number
      COMPUTE SECRETNUMBER = Pseudo-Random-Number * 100
      DISPLAY SECRETNUMBER . *> TK remember to remove this

    ENTERUSERGUESS. *> Start the guessing loop

    *> Print a description of the game, with rules, to the screen
    *> TK - print complete instructions with nice formatting
    DISPLAY "Guess the number between 1 and 100.".

    ACCEPT USERGUESS.

    ADD 1 TO TOTALGUESSES.

    *> ***********************************************************
    *> Input validation section
    *> ***********************************************************

    *> Non-numerics read as 0, so this checks for guesses like "F" or
    *> "throw mamma from the train" and rejects them.
    IF USERGUESS = 0
      DISPLAY "Guesses must be integers between 1 and 100."
      GO To ENTERUSERGUESS
      END-IF.

    *> Check if positive integer - TK

    IF USERGUESS > 100
      DISPLAY "Guesses must be between 1 and 100."
      GO TO ENTERUSERGUESS
      END-IF.

    IF USERGUESS < 1
      DISPLAY "Guesses must be between 1 and 100."
      GO TO ENTERUSERGUESS
      END-IF.

    *> ***********************************************************
    *> Taunts for when it's taking too long
    *> ***********************************************************

    IF TOTALGUESSES = 8
      DISPLAY "This is a hard number, isn't it?"
      END-IF.

    IF TOTALGUESSES = 12
      DISPLAY "Wow! You are really bad at this."
      END-IF.

    IF TOTALGUESSES = 16
      DISPLAY "You're taking too long, I can't handle it any more. GAME OVER."
      STOP RUN
      END-IF.

   *> ***********************************************************
   *> Taunts for carelessness - they lose their turn
   *> ***********************************************************

    IF USERGUESS < LOWMAX
      DISPLAY "That guess was lower than a previous guess that was too low. Pay attention!"
      GO TO ENTERUSERGUESS
      END-IF.

    IF USERGUESS > HIGHMAX
      DISPLAY "Wake up! That guess was higher than an earlier guess that was too high."
      GO TO ENTERUSERGUESS
      END-IF.

    *> ***********************************************************
    *> Evaluate guess against answer
    *> ***********************************************************

    IF USERGUESS > SECRETNUMBER
      DISPLAY "Your guess is too high! Guess again."
      IF USERGUESS <= HIGHMAX
        *> make highmax equal userguess minus one
        COMPUTE HIGHMAX = USERGUESS - 1
        END-IF
      GO TO ENTERUSERGUESS
      END-IF.

    IF USERGUESS < SECRETNUMBER
      DISPLAY "Your guess is too low! Guess again."
      *> make lowmax equal userguess plus one
      COMPUTE LOWMAX = USERGUESS + 1
      GO TO ENTERUSERGUESS
      END-IF.

    DISPLAY "You got it! Total guesses:".
    DISPLAY TOTALGUESSES.
    STOP RUN.

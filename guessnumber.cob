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
*> Copyright (c) 2024 Matthew Helmke for the COBOL version (this one)
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

*> I've forgotten so much and GnuCOBOL changed some of what I do recall. LOL.

 IDENTIFICATION DIVISION.
 PROGRAM-ID. GuessMyNumber.

*> ***************************************************************

 DATA DIVISION.
 WORKING-STORAGE SECTION.
    *> Define variables
    01 USERGUESS PIC 999 usage comp-6. *> unsigned (positive) packed decimal
                                       *> defined by the number of 9s
    01 COMPUTERGUESS PIC 999 usage comp-6.
    01 SECRETNUMBER PIC 999 usage comp-6.
    01 GUESSRANGE PIC 999 usage comp-6.
    01 TOTALGUESSES PIC 99 usage comp-6.
    01 LOWMAX PIC 99 usage comp-6.
    01 HIGHMAX PIC 999 usage comp-6.
    01 SEED PIC 999999999 usage comp-6.
    01 PSEUDO-RANDOM-NUMBER usage comp-1. *> float-short

*> ***************************************************************

 PROCEDURE DIVISION.
    *> Assign values to some variables
    COMPUTE TOTALGUESSES = 0.
    COMPUTE LOWMAX = 0.
    COMPUTE HIGHMAX = 100.
    MOVE FUNCTION CURRENT-DATE(1:16) to SEED.
    *> FUNCTION RANDOM is pseudo-random, not true random, but good enough
    *> for this game. Using the date, formatted as a number, provides a
    *> constantly-changing seed value, which helps.
    ComputeSecretNumber.
      MOVE FUNCTION RANDOM(SEED) TO PSEUDO-RANDOM-NUMBER
      COMPUTE SECRETNUMBER = PSEUDO-RANDOM-NUMBER * 100 .

    *> Print a description of the game, with rules, to the screen
    DISPLAY "Welcome to Guess My Number!".
    DISPLAY " ".
    DISPLAY "The computer will select a random whole number between 1 and 100.".
    DISPLAY "Your goal is to guess that number. You will get a turn, then a computer".
    DISPLAY "player will get a turn. Each of you are aware of the other's guesses.".
    DISPLAY "The first one to guess the number correctly will win. Try to guess in".
    DISPLAY "as few turns as possible. NOTE: Negative numbers are automatically".
    DISPLAY "changed to positive before being evaluated."
    DISPLAY " ".
    DISPLAY "Here we go!".
    DISPLAY " ".

    ENTERUSERGUESS. *> Start the guessing loop, which continues to the file end

    DISPLAY "What is your guess?".

    ACCEPT USERGUESS.

    ADD 1 TO TOTALGUESSES.

    DISPLAY "User guessed: " USERGUESS.

    *> ***********************************************************
    *> Input validation section
    *> ***********************************************************

    *> Non-numerics read as 0, so this checks for guesses like "F" or
    *> "throw mamma from the train" and rejects them.
    *> NO IDEA why "USERGUESS IS NOT NUMERIC" didn't work; glad this does.
    IF USERGUESS = 0
      DISPLAY "Guesses must be integers between 1 and 100."
      GO To ENTERUSERGUESS
      END-IF.

    *> Because of the data type used for USERGUESS, we don't need a check to
    *> make sure the guess is positive...negative values are made positive
    *> with comp-6.

    *> Missing a working check or checks to see if input is an integer

    IF USERGUESS IS NEGATIVE
      DISPLAY "Guesses must be integers between 1 and 100."
      GO To ENTERUSERGUESS
      END-IF.

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
      GO TO CALCULATECOMPUTERGUESS
      END-IF.

    IF USERGUESS > HIGHMAX
      DISPLAY "Wake up! That guess was higher than an earlier guess that was too high."
      GO TO CALCULATECOMPUTERGUESS
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
      GO TO CALCULATECOMPUTERGUESS
      END-IF.

    IF USERGUESS < SECRETNUMBER
      DISPLAY "Your guess is too low! Guess again."
      *> make lowmax equal userguess plus one
      COMPUTE LOWMAX = USERGUESS + 1
      GO TO CALCULATECOMPUTERGUESS
      END-IF.

    IF USERGUESS = SECRETNUMBER
      DISPLAY "Your guess is correct! Congratulations!!".
      DISPLAY "Total guesses: " TOTALGUESSES.
      STOP RUN.


    CALCULATECOMPUTERGUESS.
      ADD 1 TO TOTALGUESSES.
      COMPUTE GUESSRANGE = HIGHMAX - LOWMAX
      *> FUNCTION RANDOM is pseudo-random, not true random, but good enough
      *> for this game. What helps is that I adjust the computer guesses to
      *> fit within the guessrange, so the parameters are always changing.
      *> I believe the function RANDOM uses the same seed throughout after the
      *> seed is used once, which it is in the initial SECRETNUMBER generation.
      MOVE FUNCTION RANDOM(LOWMAX,HIGHMAX) to COMPUTERGUESS.

      DISPLAY "The computer guessed: " COMPUTERGUESS.

      IF COMPUTERGUESS > SECRETNUMBER
        DISPLAY "The computer's guess is too high."
        DISPLAY " "
        IF COMPUTERGUESS <= HIGHMAX
          *> make highmax equal userguess minus one
          COMPUTE HIGHMAX = COMPUTERGUESS - 1
          GO TO ENTERUSERGUESS
          END-IF
        GO TO ENTERUSERGUESS
        END-IF.

      IF COMPUTERGUESS < SECRETNUMBER
        DISPLAY "The computer's guess is too low."
        DISPLAY " "
        IF COMPUTERGUESS >= LOWMAX
          *> make lowmax equal userguess plus one
          COMPUTE LOWMAX = COMPUTERGUESS + 1
          GO TO ENTERUSERGUESS
          END-IF
        GO TO ENTERUSERGUESS
        END-IF.

      DISPLAY "The computer guessed correctly!".
      DISPLAY " ".
      DISPLAY "Total guesses: " TOTALGUESSES.
      STOP RUN.

    STOP RUN.

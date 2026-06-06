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

*> I've forgotten so much and GnuCOBOL changed some of what I do recall. LOL.

 IDENTIFICATION DIVISION.
 PROGRAM-ID. GuessMyNumber.

*> ***************************************************************

 DATA DIVISION.
 WORKING-STORAGE SECTION.
    *> Define variables
    01 USERGUESS PIC S9(3) usage comp-3. *> signed packed decimal allows for negative values, which we will use to detect non-numeric input, which is converted to 0 by comp-3, and then made negative by the signed part of the data type. This allows us to reject non-numeric input with a specific error message about non-numeric input, rather than just rejecting it as an out-of-range guess.
    01 COMPUTERGUESS PIC 999 usage comp-6. *> unsigned (positive) packed decimal
    01 WS-COMPUTERGUESS-TRIMMED PIC ZZZ.
    01 SECRETNUMBER PIC 999 usage comp-6. *> unsigned (positive) packed decimal
    01 TOTALGUESSES PIC 99 usage comp-6. *> unsigned (positive) packed decimal
    01 WS-TOTALGUESSES-TRIMMED PIC Z9.
    01 LOWMAX PIC 99 usage comp-6. *> unsigned (positive) packed decimal
    01 HIGHMAX PIC 999 usage comp-6. *> unsigned (positive) packed decimal
    01 SEED PIC 999999999 usage comp-6. *> unsigned (positive) packed decimal
    01 PSEUDO-RANDOM-NUMBER usage comp-1. *> float-short
    01 WS-USER-INPUT PIC X(10). *> raw line from the keyboard
    01 WS-USER-GUESS PIC S9(3) COMP-3. *> guess converted to a number
    01 WS-GMN-SECRET PIC X(10). *> raw GMN_SECRET env value, for parity testing
    01 WS-GMN-SECRET-NUM PIC 9(9). *> GMN_SECRET parsed to a number

*> ***************************************************************

 PROCEDURE DIVISION.
    *> Assign values to some variables
    COMPUTE TOTALGUESSES = 0.
    COMPUTE LOWMAX = 1.
    COMPUTE HIGHMAX = 100.
    MOVE FUNCTION CURRENT-DATE(1:16) to SEED.
    *> FUNCTION RANDOM is pseudo-random, not true random, but good enough
    *> for this game. Using the date, formatted as a number, provides a
    *> constantly-changing seed value, which helps.
    ComputeSecretNumber.
      *> Use a fixed secret from GMN_SECRET when set, for parity testing
      MOVE SPACES TO WS-GMN-SECRET
      ACCEPT WS-GMN-SECRET FROM ENVIRONMENT "GMN_SECRET"
      COMPUTE WS-GMN-SECRET-NUM = FUNCTION NUMVAL(WS-GMN-SECRET)
      IF WS-GMN-SECRET-NUM >= 1 AND WS-GMN-SECRET-NUM <= 100
        COMPUTE SECRETNUMBER = WS-GMN-SECRET-NUM
      ELSE
        MOVE FUNCTION RANDOM(SEED) TO PSEUDO-RANDOM-NUMBER
        COMPUTE SECRETNUMBER = PSEUDO-RANDOM-NUMBER * 100
      END-IF.

    *> Print a description of the game, with rules, to the screen
    DISPLAY "Welcome to Guess My Number!".
    DISPLAY " ".
    DISPLAY "The computer will select a random whole number between 1 and 100.".
    DISPLAY "Your goal is to guess that number. You will get a turn, then a computer".
    DISPLAY "player will get a turn. Each of you are aware of the other's guesses.".
    DISPLAY "The first one to guess the number correctly will win. Try to guess in".
    DISPLAY "as few turns as possible.".
    DISPLAY " ".
    DISPLAY "Here we go!".
    DISPLAY " ".

    ENTERUSERGUESS. *> Start the guessing loop, which continues to the file end

    DISPLAY "What is your guess? " WITH NO ADVANCING.

    ACCEPT WS-USER-INPUT.

    *> ***********************************************************
    *> Input validation section
    *> ***********************************************************

    *> verify the guess is a whole number (TRIM removes the trailing spaces
    *> the PIC X(10) field adds, which would otherwise fail NUMERIC)
    IF FUNCTION TRIM (WS-USER-INPUT) IS NUMERIC
      MOVE FUNCTION NUMVAL (WS-USER-INPUT) TO WS-USER-GUESS
    ELSE
      DISPLAY "Only whole numbers from 1 to 100 are allowed."
      DISPLAY "Please try again."
      DISPLAY " "
      GO TO ENTERUSERGUESS
    END-IF.

    IF FUNCTION INTEGER (WS-USER-GUESS) NOT = WS-USER-GUESS
      DISPLAY "Only whole numbers from 1 to 100 are allowed."
      DISPLAY "Please try again."
      DISPLAY " "
      GO TO ENTERUSERGUESS
    END-IF.

    *> verify the guess is in range
    IF WS-USER-GUESS < 1 OR WS-USER-GUESS > 100
      DISPLAY "Only whole numbers from 1 to 100 are allowed. "
        "Your guess is out of range."
      DISPLAY "Please try again."
      DISPLAY " "
      GO TO ENTERUSERGUESS
    END-IF.

    MOVE WS-USER-GUESS TO USERGUESS.

    *> this is a real guess, so count it
    ADD 1 TO TOTALGUESSES.

    *> some taunts for silly errors in user guesses
    IF USERGUESS < LOWMAX
      DISPLAY "That guess was lower than a previous guess that was too low. "
        "Pay attention!"
      DISPLAY " "
    END-IF.

    IF USERGUESS > HIGHMAX
      DISPLAY "Wake up! That guess was higher than an earlier guess that "
        "was too high."
      DISPLAY " "
    END-IF.

    *> ***********************************************************
    *> Evaluate guess against answer
    *> ***********************************************************

    IF USERGUESS = SECRETNUMBER
      MOVE TOTALGUESSES TO WS-TOTALGUESSES-TRIMMED
      DISPLAY " "
      DISPLAY "*********************************************"
      DISPLAY "   Your guess is correct! Congratulations!"
      DISPLAY "   It took " FUNCTION TRIM (WS-TOTALGUESSES-TRIMMED)
        " total guesses."
      DISPLAY "*********************************************"
      DISPLAY " "
      STOP RUN
    END-IF.

    IF USERGUESS > SECRETNUMBER
      DISPLAY "Your guess is too high."
      DISPLAY " "
      IF USERGUESS <= HIGHMAX
        COMPUTE HIGHMAX = USERGUESS - 1
      END-IF
    ELSE
      DISPLAY "Your guess is too low."
      DISPLAY " "
      IF USERGUESS >= LOWMAX
        COMPUTE LOWMAX = USERGUESS + 1
      END-IF
    END-IF.

    CALCULATECOMPUTERGUESS.
      *> computer uses the midpoint (binary search) within current bounds
      COMPUTE COMPUTERGUESS = (LOWMAX + HIGHMAX) / 2
      ADD 1 TO TOTALGUESSES
      MOVE COMPUTERGUESS TO WS-COMPUTERGUESS-TRIMMED

      IF COMPUTERGUESS = SECRETNUMBER
        MOVE TOTALGUESSES TO WS-TOTALGUESSES-TRIMMED
        DISPLAY "**********************************************"
        DISPLAY "   The computer's guess of "
          FUNCTION TRIM (WS-COMPUTERGUESS-TRIMMED) " is correct!"
        DISPLAY "   It took " FUNCTION TRIM (WS-TOTALGUESSES-TRIMMED)
          " total guesses."
        DISPLAY "**********************************************"
        DISPLAY " "
        STOP RUN
      END-IF.

      IF COMPUTERGUESS > SECRETNUMBER
        DISPLAY "The computer guessed "
          FUNCTION TRIM (WS-COMPUTERGUESS-TRIMMED) " and that was too high."
        DISPLAY "Please try again."
        DISPLAY " "
        COMPUTE HIGHMAX = COMPUTERGUESS - 1
      ELSE
        DISPLAY "The computer guessed "
          FUNCTION TRIM (WS-COMPUTERGUESS-TRIMMED) " and that was too low."
        DISPLAY "Please try again."
        DISPLAY " "
        COMPUTE LOWMAX = COMPUTERGUESS + 1
      END-IF.

      *> more taunts and a forced guess limit
      EVALUATE TRUE
        WHEN TOTALGUESSES = 8
          DISPLAY " "
          DISPLAY "This is a hard number, isn't it?"
          DISPLAY " "
        WHEN TOTALGUESSES = 12
          DISPLAY " "
          DISPLAY "Wow! You are really bad at this."
          DISPLAY " "
        WHEN TOTALGUESSES >= 16
          DISPLAY " "
          DISPLAY "You're taking too long, I can't handle it any more."
          DISPLAY " "
          DISPLAY "G A M E   O V E R"
          STOP RUN
      END-EVALUATE.

      GO TO ENTERUSERGUESS.

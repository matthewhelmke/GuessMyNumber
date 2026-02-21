10 REM Guess my number game
20 REM
30 REM --A BASIC version of a silly game I made on my programmable
40 REM   calculator when I was bored in math class in 1987, with a couple of
50 REM   additions like input validation and computer guesses.
55 REM   The first program I wrote in BASIC, so this was nostalgic.
60 REM
70 REM Copyright (c) 2007 Matthew Helmke for the old Python 2 version
80 REM Copyright (c) 2026 Matthew Helmke for the BASIC version (this one)
90 REM
100 REM To run (with PC-BASIC):
110 REM   pcbasic guessnumber.bas
120 REM
130 REM This program is free software; you can redistribute it and\or
140 REM modify it under the terms of the GNU General Public License
150 REM as published by the Free Software Foundation; either version 2
160 REM of the License, or (at your option) any later version.
170 REM
180 REM This program is distributed in the hope that it will be useful,
190 REM but WITHOUT ANY WARRANTY; without even the implied warranty of
200 REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
210 REM GNU General Public License for more details.
220 REM
230 REM You should have received a copy of the GNU General Public License
240 REM along with this program; if not, write to the Free Software
250 REM Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

255 REM Initialize random number generator
260 RANDOMIZE TIMER

270 REM Set initial values
280 LET LOWMAX = 1
290 LET HIGHMAX = 100
300 LET TOTALGUESSES = 0

310 REM Generate secret number
320 LET SECRETNUMBER = INT(RND * 100) + 1

330 REM Print welcome message
340 PRINT "Welcome to Guess My Number!"
350 PRINT
360 PRINT "The computer will select a random whole number between 1 and 100."
370 PRINT "Your goal is to guess that number. You will get a turn, then a computer"
380 PRINT "player will get a turn. Each of you are aware of the other's guesses."
390 PRINT "The first one to guess the number correctly will win. Try to guess in"
400 PRINT "as few turns as possible."
410 PRINT
420 PRINT "Here we go!"
430 PRINT

440 REM Main game loop
450 INPUT "What is your guess"; USERGUESS

460 REM Validate input is numeric and in range
470 IF USERGUESS < 1 OR USERGUESS > 100 THEN PRINT "Only whole numbers from 1 to 100 are allowed.": PRINT "Please try again.": GOTO 450

530 LET TOTALGUESSES = TOTALGUESSES + 1

540 REM Check for silly errors
550 IF USERGUESS < LOWMAX THEN PRINT "That guess was lower than a previous guess that was too low. Pay attention!"
560 IF USERGUESS > HIGHMAX THEN PRINT "Wake up! That guess was higher than an earlier guess that was too high."

610 REM Evaluate user guess
620 IF USERGUESS < SECRETNUMBER THEN PRINT "Your guess is too low.": LET LOWMAX = USERGUESS + 1: GOTO 760
630 IF USERGUESS > SECRETNUMBER THEN PRINT "Your guess is too high.": LET HIGHMAX = USERGUESS - 1: GOTO 760
640 PRINT
650 PRINT "*********************************************"
660 PRINT "   Your guess is correct! Congratulations!"
670 PRINT "   It took"; TOTALGUESSES; "total guesses."
680 PRINT "*********************************************"
690 END

760 REM Computer turn
770 LET COMPUTERGUESSES = INT((LOWMAX + HIGHMAX) / 2)
780 LET TOTALGUESSES = TOTALGUESSES + 1

790 PRINT "The computer guessed"; COMPUTERGUESSES

800 REM Evaluate computer guess
810 IF COMPUTERGUESSES < SECRETNUMBER THEN PRINT "The computer's guess is too low.": LET LOWMAX = COMPUTERGUESSES + 1: GOTO 950
820 IF COMPUTERGUESSES > SECRETNUMBER THEN PRINT "The computer's guess is too high.": LET HIGHMAX = COMPUTERGUESSES - 1: GOTO 950
830 PRINT
840 PRINT "*********************************************"
850 PRINT "   The computer's guess of"; COMPUTERGUESSES; "is correct!"
860 PRINT "   It took"; TOTALGUESSES; "total guesses."
870 PRINT "*********************************************"
880 END

950 REM Taunts
960 IF TOTALGUESSES = 8 THEN PRINT : PRINT "Is this a hard number?"
970 IF TOTALGUESSES = 12 THEN PRINT : PRINT "Wow! You are really bad at this."
980 IF TOTALGUESSES >= 16 THEN PRINT : PRINT "You're taking too long, I can't handle it any more.": PRINT : PRINT "G A M E   O V E R": END

1110 GOTO 450
#lang racket

; Guess my number game
; 
; --A Racket version of a silly game I made on my programmable
;   calculator when I was bored in math class in 1987, with a couple of
;   additions like input validation and computer guesses.
;
; Copyright (c) 2007 Matthew Helmke for the old Python 2 version
; Copyright (c) 2021 Matthew Helmke for the Racket version (this one)
;
; To run:
;   racket guessnumber.rkt
;
; This program is free software; you can redistribute it and\or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

; Set variables
(define (inquire-user number)

; the main bit
(display "What is your guess? ")
(define userguess (string->number (read-line)))
; Validate input against multiple criteria
(cond [(not (integer? userguess)) (displayln "Only whole numbers from 1 to 100 are allowed. Please try again. ") (inquire-user number)]
      [(< userguess 1) (displayln "Only whole numbers from 1 to 100 are allowed.  Your guess is out of range. Please try again. ") (inquire-user number)]
      [(> userguess 100) (displayln "Only whole numbers from 1 to 100 are allowed.  Your guess is out of range. Please try again. ") (inquire-user number)]
      [(> number userguess) (displayln "Your guess is too low.") (inquire-user number)]
      [(< number userguess) (displayln "Your guess is too high.") (inquire-user number)]
      [else (displayln "Correct!")]))

; Print a description of the game, with rules, to the screen
(displayln "Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!")

; Start the process by getting user input for the first time and comparing to a random number
; then go back up to cond line for first comparison and additional input if not high or low
(inquire-user (random 1 101))
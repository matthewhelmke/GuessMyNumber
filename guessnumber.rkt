#lang racket

; Guess my number game
; 
; --A Racket version of a silly game I made on my programmable
;   calculator when I was bored in math class in 1987, with a couple of
;   additions like input validation and computer guesses.
;
; Copyright (c) 2007 Matthew Helmke for the old Python 2 version
; Copyright (c) 2024 Matthew Helmke for the Racket version (this one)
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

; Print a description of the game, with rules, to the screen
(printf "Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!\n\n")

; Define initial values
(define computerguess (random 1 101))
(define highmax 100)
(define lowmax 1)
(define totalguesses 0)

; Define the guessing function, with validation rules
(define (inquire-user secretnumber)
  (printf "What is your guess? ")
  (define userguess (string->number (read-line)))
  ; Validate userguess against multiple criteria
  (set! totalguesses (add1 totalguesses))
  (cond [(not (integer? userguess)) (printf "Only whole numbers from 1 to 100 are allowed. Please try again. ") (inquire-user secretnumber)]
        [(< userguess 1) (printf "Only whole numbers from 1 to 100 are allowed.  Your guess is out of range. Please try again. ") (inquire-user secretnumber)]
        [(> userguess 100) (printf "Only whole numbers from 1 to 100 are allowed.  Your guess is out of range. Please try again. ") (inquire-user secretnumber)]
        ; A couple taunts
        [(< userguess lowmax) (printf "That guess was lower than a previous guess that was too low. Pay attention!\n")]
        [(> userguess highmax) (printf "Wake up! That guess was higher than an earlier guess that was too high.\n")]
        [(> secretnumber userguess) (printf "Your guess is too low.\n") (set! lowmax userguess)]
        [(< secretnumber userguess) (printf "Your guess is too high.\n") (set! highmax userguess)]
        [(= secretnumber userguess) (printf "\n*********************************************\nYour guess is correct! Congratulations!\nIt took ~a turns.\n*********************************************\n" totalguesses) (exit)])
  ; Computer gets a turn
  (set! totalguesses (add1 totalguesses))
  (set! computerguess (+ (random (- highmax lowmax)) lowmax))
  (cond [(> secretnumber computerguess) (printf "The computer guessed ~a and that is too low.\n" computerguess) (set! lowmax computerguess)]
        [(< secretnumber computerguess) (printf "The computer guessed ~a and that is too high.\n" computerguess) (set! highmax computerguess)]
        [(= secretnumber computerguess) (printf "\n*********************************************\nThe computer guess of ~a is correct!\nIt took ~b turns.\n**********************************************\n" computerguess totalguesses) (exit)])
  ; Taunts based on taking a long time to guess correctly
  (cond [(= totalguesses 8) (printf "\nThis is a hard number, isn't it?\n")]
        [(= totalguesses 12) (printf "\nWow! You are really bad at this.\n")]
        [(= totalguesses 16) (printf "\nYou're taking too long, I can't handle it any more.\nG A M E   O V E R\n") (exit)])
  
  (inquire-user secretnumber))

; Call the function with "secretnumber" defined as a random number between 1 and 100
(inquire-user (random 1 101))
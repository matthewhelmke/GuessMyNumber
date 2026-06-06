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

; Print a description of the game, with rules, to the screen
(printf "Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!\n\n")

; Define initial values
(define computerguess 0)
(define highmax 100)
(define lowmax 1)
(define totalguesses 0)

; Define the guessing function, with validation rules
(define (inquire-user secretnumber)
  (printf "What is your guess? ")
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]   ; end of input
    [(not (regexp-match? #rx"^[0-9]+$" (string-trim line)))
     ; not a whole number
     (printf "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n\n")
     (inquire-user secretnumber)]
    [else
     (define userguess (string->number (string-trim line)))
     (cond
       [(or (< userguess 1) (> userguess 100))
        ; out of range
        (printf "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n\n")
        (inquire-user secretnumber)]
       [else
        ; this is a real guess, so count it
        (set! totalguesses (add1 totalguesses))

        ; some taunts for silly errors in user guesses
        (when (< userguess lowmax)
          (printf "That guess was lower than a previous guess that was too low. Pay attention!\n\n"))
        (when (> userguess highmax)
          (printf "Wake up! That guess was higher than an earlier guess that was too high.\n\n"))

        ; evaluate the guess
        (cond
          [(= userguess secretnumber)
           (printf "\n*********************************************\n   Your guess is correct! Congratulations!\n   It took ~a total guesses.\n*********************************************\n\n" totalguesses)
           (exit)]
          [(> userguess secretnumber)
           (printf "Your guess is too high.\n\n")
           (when (<= userguess highmax) (set! highmax (- userguess 1)))]
          [else
           (printf "Your guess is too low.\n\n")
           (when (>= userguess lowmax) (set! lowmax (+ userguess 1)))])

        ; computer uses the midpoint (binary search) within current bounds
        (set! computerguess (quotient (+ lowmax highmax) 2))
        (set! totalguesses (add1 totalguesses))
        (cond
          [(= computerguess secretnumber)
           (printf "**********************************************\n   The computer's guess of ~a is correct!\n   It took ~a total guesses.\n**********************************************\n\n" computerguess totalguesses)
           (exit)]
          [(> computerguess secretnumber)
           (printf "The computer guessed ~a and that was too high.\nPlease try again.\n\n" computerguess)
           (set! highmax (- computerguess 1))]
          [else
           (printf "The computer guessed ~a and that was too low.\nPlease try again.\n\n" computerguess)
           (set! lowmax (+ computerguess 1))])

        ; more taunts and a forced guess limit
        (cond
          [(= totalguesses 8) (printf "\nThis is a hard number, isn't it?\n\n")]
          [(= totalguesses 12) (printf "\nWow! You are really bad at this.\n\n")]
          [(>= totalguesses 16)
           (printf "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n")
           (exit)])

        (inquire-user secretnumber)])]))

; Call the function with "secretnumber" set from GMN_SECRET (for parity
; testing) or, failing that, a random number between 1 and 100
(define gmn-secret (let ([e (getenv "GMN_SECRET")])
                     (and e (string->number e))))
(inquire-user (if (and (exact-integer? gmn-secret) (<= 1 gmn-secret 100))
                  gmn-secret
                  (random 1 101)))
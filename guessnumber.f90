! Guess my number game
!
! --A Fortran version of a silly game I made on my programmable
!   calculator when I was bored in math class in 1987, with a couple of
!   additions like input validation and computer guesses.
!
! Copyright (c) 2007 Matthew Helmke for the old Python 2 version
! Copyright (c) 2025 Matthew Helmke for the Fortran version (this one)
!
! To compile:
!    gfortran guessnumber.f90 -o guessnumber
! To run:
!   ./guessnumber
!
! This program is free software; you can redistribute it and\or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

! Print a description of the game, with rules, to the screen

PROGRAM GuessMyNumber
  IMPLICIT none
  INTEGER :: secret, guess, playerGuess, computerGuess, totalGuesses
  INTEGER :: playerTries, computerTries
  INTEGER :: seed, low, high
  CHARACTER(len=100) :: input
  INTEGER :: iostat_val
  INTEGER :: random_integer
  REAL :: random_real

  ! Initialize the random number generator (call once at program start)
  CALL RANDOM_SEED()

  ! Generate a real random number between 0.0 and 1.0
  CALL RANDOM_NUMBER(random_real)

  ! Scale and convert to an integer between 1 and 100
  secret = INT(random_real * 100) + 1


  PRINT *, "Welcome to Guess My Number!"
  PRINT *
  PRINT *, "The computer will select a random whole number between 1 and 100."
  PRINT *, "Your goal is to guess that number. You will get a turn, then a computer"
  PRINT *, "player will get a turn. Each of you are aware of the other's guesses."
  PRINT *, "The first one to guess the number correctly will win. Try to guess in"
  PRINT *, "as few turns as possible."
  PRINT *
  PRINT *, "Here we go!"

  ! Initialize counters
  playerTries = 0
  computerTries = 0
  low = 1
  high = 100

  DO
    ! Player's turn
    PRINT *, "What is your guess? "

    ! Verify the guess is an integer between 1 and 100
    DO
      READ(*, '(A)') input
      READ(input, *, iostat=iostat_val) playerGuess
      IF (iostat_val == 0) THEN  ! Check if read was successful (no I/O error)
        IF (playerGuess >= 1 .AND. playerGuess <= 100) THEN
          WRITE(*,*) "Valid input:", playerGuess
          EXIT  ! Exit the loop if input is valid
        ELSE
          WRITE(*,*) "Only whole numbers from 1 to 100 are allowed. Your guess is out of range. Try again."
        END IF
      ELSE
        WRITE(*,*) "Only whole numbers from 1 to 100 are allowed. Please try again."
      END IF
    END DO

    ! some taunts for silly errors in user guesses
    IF (playerGuess < low) THEN
      PRINT *, "That guess was lower than a previous guess that was too low. Pay attention!"
    ELSE IF (playerGuess > high) THEN
      PRINT *, "Wake up! That guess was higher than an earlier guess that was too high."
    END IF

    playerTries = playerTries + 1

    ! Evaluate player guess against secret
    IF (playerGuess < secret) THEN
      PRINT *, "Too low!"
      low = playerGuess + 1
    ELSE IF (playerGuess > secret) THEN
      PRINT *, "Too high!"
      high = playerGuess - 1
    ELSE
      totalGuesses = playerTries + computerTries
      PRINT *, "*********************************************"
      PRINT *, "Your guess is correct! Congratulations!"
      PRINT *, "It took", totalGuesses, "tries!"
      PRINT *, "*********************************************"
      EXIT
    END IF

    ! Computer's turn
    computerGuess = (low + high) / 2
    PRINT *, "Computer guesses: ", computerGuess
    computerTries = computerTries + 1

    ! Evaluate computer guess against secret
    if (computerGuess < secret) THEN
      PRINT *, "Computer: Too low!"
      low = computerGuess + 1
    ELSE IF (computerGuess > secret) THEN
      PRINT *, "Computer: Too high!"
      high = computerGuess - 1
    ELSE
      totalGuesses = playerTries + computerTries
      PRINT *, "*********************************************"
      PRINT *, "The computer's guess of", computerGuess, "is correct!"
      PRINT *, "It took", totalGuesses, "tries!"
      PRINT *, "*********************************************"
      EXIT
    END IF

    ! these taunts are for my amusement and to keep the game from being too long
    IF (totalGuesses == 8) THEN
      PRINT *, "Is this a hard number?"
    ELSE IF (totalGuesses == 12) THEN
      PRINT *, "Wow! You are really bad at this."
    ELSE IF (totalGuesses >= 16) THEN
      PRINT *, "This is taking too long."
      PRINT *, "G A M E   O V E R"
      EXIT
    END IF

  END DO
END PROGRAM GuessMyNumber

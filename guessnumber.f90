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

PROGRAM GuessMyNumber
  IMPLICIT none
  INTEGER :: secretnumber, userguess, computerguess, totalguesses, lowmax, highmax
  CHARACTER(len=100) :: input
  INTEGER :: iostat_val
  REAL :: random_real

  ! Initialize the random number generator (call once at program start)
  CALL RANDOM_SEED()

  ! Generate a real random number between 0.0 and 1.0
  CALL RANDOM_NUMBER(random_real)

  ! Scale and convert to an integer between 1 and 100
  secretnumber = INT(random_real * 100) + 1
  
  ! Initialize counters
  totalguesses = 0
  lowmax = 1
  highmax = 100

  PRINT *, "Welcome to Guess My Number!"
  PRINT *
  PRINT *, "The computer will select a random whole number between 1 and 100."
  PRINT *, "Your goal is to guess that number. You will get a turn, then a computer"
  PRINT *, "player will get a turn. Each of you are aware of the other's guesses."
  PRINT *, "The first one to guess the number correctly will win. Try to guess in"
  PRINT *, "as few turns as possible."
  PRINT *
  PRINT *, "Here we go!"

  DO
    ! Player's turn
    PRINT *, "What is your guess? "

    totalguesses = totalguesses + 1

    ! Verify the guess is an integer between 1 and 100
    DO
      READ(*, '(A)', iostat=iostat_val) input
      IF (iostat_val /= 0) THEN
        PRINT *, "End of input. Game Over."
        STOP
      END IF
      READ(input, *, iostat=iostat_val) userguess
      IF (iostat_val == 0) THEN  ! Check if read was successful
        IF (userguess >= 1 .AND. userguess <= 100) THEN
          EXIT  ! Exit the loop if input is valid
        ELSE
          PRINT *, "Only whole numbers from 1 to 100 are allowed. Please try again."
        END IF
      ELSE
        PRINT *, "Only whole numbers from 1 to 100 are allowed. Please try again."
      END IF
    END DO

    ! some taunts for silly errors in user guesses
    IF (userguess < lowmax) THEN
      PRINT *, "That guess was lower than a previous guess that was too low. Pay attention!"
    ELSE IF (userguess > highmax) THEN
      PRINT *, "Wake up! That guess was higher than an earlier guess that was too high."
    END IF

    ! Evaluate player guess against secretnumber
    IF (userguess < secretnumber) THEN
      PRINT *, "Too low!"
      lowmax = userguess + 1
    ELSE IF (userguess > secretnumber) THEN
      PRINT *, "Too high!"
      highmax = userguess - 1
    ELSE
      PRINT *, "*********************************************"
      PRINT *, "Your guess is correct! Congratulations!"
      PRINT *, "It took", totalguesses, "total guesses."
      PRINT *, "*********************************************"
      EXIT
    END IF

    ! Check for game over condition
    IF (totalguesses >= 16) THEN
      PRINT *, "You're taking too long, I can't handle it any more."
      PRINT *, ""
      PRINT *, "G A M E   O V E R"
      STOP
    END IF

    ! Computer's turn
    computerguess = (lowmax + highmax) / 2
    totalguesses = totalguesses + 1

    ! Evaluate computer guess against secret
    IF (computerguess < secretnumber) THEN
      PRINT *, "The computer guessed", computerguess, "and that was too low."
      lowmax = computerguess + 1
    ELSE IF (computerguess > secretnumber) THEN
      PRINT *, "The computer guessed", computerguess, "and that was too high."
      highmax = computerguess - 1
    ELSE
      PRINT *, "*********************************************"
      PRINT *, "The computer's guess of", computerguess, "is correct!"
      PRINT *, "It took", totalguesses, "total guesses."
      PRINT *, "*********************************************"
      EXIT
    END IF

    ! these taunts are for my amusement and to keep the game from being too long
    IF (totalguesses == 8) THEN
      PRINT *, "This is a hard number, isn't it?"
    ELSE IF (totalguesses == 12) THEN
      PRINT *, "Wow! You are really bad at this."
    ELSE IF (totalguesses >= 16) THEN
      PRINT *, "You're taking too long, I can't handle it any more."
      PRINT *, ""
      PRINT *, "G A M E   O V E R"
      STOP
    END IF

  END DO
END PROGRAM GuessMyNumber

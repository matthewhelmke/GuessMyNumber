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
  ! Variables for the GMN_SECRET parity-testing hook
  CHARACTER(len=100) :: env_secret
  INTEGER :: env_len, env_status, env_iostat, gmn_secret_val

  ! Initialize the random number generator (call once at program start)
  CALL RANDOM_SEED()

  ! Use a fixed secret from GMN_SECRET when set (for parity testing),
  ! otherwise a random integer between 1 and 100
  gmn_secret_val = 0
  env_iostat = 1
  CALL GET_ENVIRONMENT_VARIABLE("GMN_SECRET", env_secret, env_len, env_status)
  IF (env_status == 0) THEN
    READ(env_secret, *, IOSTAT=env_iostat) gmn_secret_val
  END IF
  IF (env_iostat == 0 .AND. gmn_secret_val >= 1 .AND. gmn_secret_val <= 100) THEN
    secretnumber = gmn_secret_val
  ELSE
    CALL RANDOM_NUMBER(random_real)
    secretnumber = INT(random_real * 100) + 1
  END IF
  
  ! Initialize counters
  totalguesses = 0
  lowmax = 1
  highmax = 100

  WRITE(*, '(A)') "Welcome to Guess My Number!"
  WRITE(*, '(A)') ""
  WRITE(*, '(A)') "The computer will select a random whole number between 1 and 100."
  WRITE(*, '(A)') "Your goal is to guess that number. You will get a turn, then a computer"
  WRITE(*, '(A)') "player will get a turn. Each of you are aware of the other's guesses."
  WRITE(*, '(A)') "The first one to guess the number correctly will win. Try to guess in"
  WRITE(*, '(A)') "as few turns as possible."
  WRITE(*, '(A)') ""
  WRITE(*, '(A)') "Here we go!"
  WRITE(*, '(A)') ""

  ! the main bit
  DO
    WRITE(*, '(A)', ADVANCE='NO') "What is your guess? "
    READ(*, '(A)', iostat=iostat_val) input
    IF (iostat_val /= 0) EXIT   ! end of input

    ! verify the guess is a whole number
    READ(input, *, iostat=iostat_val) userguess
    IF (iostat_val /= 0) THEN
      WRITE(*, '(A)') "Only whole numbers from 1 to 100 are allowed."
      WRITE(*, '(A)') "Please try again."
      WRITE(*, '(A)') ""
      CYCLE
    END IF

    ! verify the guess is in range
    IF (userguess < 1 .OR. userguess > 100) THEN
      WRITE(*, '(A)') "Only whole numbers from 1 to 100 are allowed. Your guess is out of range."
      WRITE(*, '(A)') "Please try again."
      WRITE(*, '(A)') ""
      CYCLE
    END IF

    ! this is a real guess, so count it
    totalguesses = totalguesses + 1

    ! some taunts for silly errors in user guesses
    IF (userguess < lowmax) THEN
      WRITE(*, '(A)') "That guess was lower than a previous guess that was too low. Pay attention!"
      WRITE(*, '(A)') ""
    END IF
    IF (userguess > highmax) THEN
      WRITE(*, '(A)') "Wake up! That guess was higher than an earlier guess that was too high."
      WRITE(*, '(A)') ""
    END IF

    ! evaluate the guess
    IF (userguess == secretnumber) THEN
      WRITE(*, '(A)') ""
      WRITE(*, '(A)') "*********************************************"
      WRITE(*, '(A)') "   Your guess is correct! Congratulations!"
      WRITE(*, '(A, I0, A)') "   It took ", totalguesses, " total guesses."
      WRITE(*, '(A)') "*********************************************"
      WRITE(*, '(A)') ""
      EXIT
    ELSE IF (userguess > secretnumber) THEN
      WRITE(*, '(A)') "Your guess is too high."
      WRITE(*, '(A)') ""
      IF (userguess <= highmax) highmax = userguess - 1
    ELSE
      WRITE(*, '(A)') "Your guess is too low."
      WRITE(*, '(A)') ""
      IF (userguess >= lowmax) lowmax = userguess + 1
    END IF

    ! computer uses the midpoint (binary search) within current bounds
    computerguess = (lowmax + highmax) / 2
    totalguesses = totalguesses + 1

    IF (computerguess == secretnumber) THEN
      WRITE(*, '(A)') "**********************************************"
      WRITE(*, '(A, I0, A)') "   The computer's guess of ", computerguess, " is correct!"
      WRITE(*, '(A, I0, A)') "   It took ", totalguesses, " total guesses."
      WRITE(*, '(A)') "**********************************************"
      WRITE(*, '(A)') ""
      EXIT
    ELSE IF (computerguess > secretnumber) THEN
      WRITE(*, '(A, I0, A)') "The computer guessed ", computerguess, " and that was too high."
      WRITE(*, '(A)') "Please try again."
      WRITE(*, '(A)') ""
      highmax = computerguess - 1
    ELSE
      WRITE(*, '(A, I0, A)') "The computer guessed ", computerguess, " and that was too low."
      WRITE(*, '(A)') "Please try again."
      WRITE(*, '(A)') ""
      lowmax = computerguess + 1
    END IF

    ! more taunts and a forced guess limit
    IF (totalguesses == 8) THEN
      WRITE(*, '(A)') ""
      WRITE(*, '(A)') "This is a hard number, isn't it?"
      WRITE(*, '(A)') ""
    ELSE IF (totalguesses == 12) THEN
      WRITE(*, '(A)') ""
      WRITE(*, '(A)') "Wow! You are really bad at this."
      WRITE(*, '(A)') ""
    ELSE IF (totalguesses >= 16) THEN
      WRITE(*, '(A)') ""
      WRITE(*, '(A)') "You're taking too long, I can't handle it any more."
      WRITE(*, '(A)') ""
      WRITE(*, '(A)') "G A M E   O V E R"
      EXIT
    END IF

  END DO
END PROGRAM GuessMyNumber

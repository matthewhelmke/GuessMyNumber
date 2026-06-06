#!/bin/bash

# Guess my number game
#
# --A Bash version of a silly game I made on my programmable
#   calculator when I was bored in math class in 1987, with a couple of
#   additions like input validation and computer guesses.
#
# Copyright (c) 2007 Matthew Helmke for the old Python 2 version
# Copyright (c) 2020 Matthew Helmke for the Bash version (this one)
#
# To run:
#   ./guessnumber.sh
#
# This program is free software; you can redistribute it and\or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

# Print a description of the game, with rules, to the screen
echo -e "Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!

"

# set all our initial values
declare -i secretnumber
declare userguessunvalidated
declare -i userguess
declare -i totalguesses=0
declare -i lowmax=1
declare -i highmax=100
declare -i computerguess

# Get a random number, or a fixed one from GMN_SECRET for parity testing
if [[ "$GMN_SECRET" =~ ^[0-9]+$ ]] && (( GMN_SECRET >= 1 && GMN_SECRET <= 100 )); then
  secretnumber=$GMN_SECRET
else
  (( secretnumber = RANDOM % 100 + 1 ))
fi

# the main bit
while :; do
  printf "What is your guess? "
  read userguessunvalidated || break   # end of input

  # verify the guess is a whole number
  if [[ ! $userguessunvalidated =~ ^[0-9]+$ ]]; then
    printf "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n\n"
    continue
  fi

  # verify the guess is in range
  userguess=$userguessunvalidated
  if ((userguess < 1 || userguess > 100)); then
    printf "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n\n"
    continue
  fi

  # this is a real guess, so count it
  ((totalguesses += 1))

  # some taunts for silly errors in user guesses
  if ((userguess < lowmax)); then
    printf "That guess was lower than a previous guess that was too low. Pay attention!\n\n"
  fi
  if ((userguess > highmax)); then
    printf "Wake up! That guess was higher than an earlier guess that was too high.\n\n"
  fi

  # evaluate the guess
  if ((userguess == secretnumber)); then
    printf "\n*********************************************\n   Your guess is correct! Congratulations!\n   It took %d total guesses.\n*********************************************\n\n" "$totalguesses"
    break
  elif ((userguess > secretnumber)); then
    printf "Your guess is too high.\n\n"
    ((userguess <= highmax)) && ((highmax = userguess - 1))
  else
    printf "Your guess is too low.\n\n"
    ((userguess >= lowmax)) && ((lowmax = userguess + 1))
  fi

  # computer uses the midpoint (binary search) within current bounds
  ((computerguess = (lowmax + highmax) / 2))
  ((totalguesses += 1))

  if ((computerguess == secretnumber)); then
    printf "**********************************************\n   The computer's guess of %d is correct!\n   It took %d total guesses.\n**********************************************\n\n" "$computerguess" "$totalguesses"
    break
  elif ((computerguess > secretnumber)); then
    printf "The computer guessed %d and that was too high.\nPlease try again.\n\n" "$computerguess"
    ((highmax = computerguess - 1))
  else
    printf "The computer guessed %d and that was too low.\nPlease try again.\n\n" "$computerguess"
    ((lowmax = computerguess + 1))
  fi

  # more taunts and a forced guess limit
  if ((totalguesses == 8)); then
    printf "\nThis is a hard number, isn't it?\n\n"
  elif ((totalguesses == 12)); then
    printf "\nWow! You are really bad at this.\n\n"
  elif ((totalguesses >= 16)); then
    printf "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n"
    break
  fi
done

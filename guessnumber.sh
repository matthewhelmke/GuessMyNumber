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
declare -i lowmax=0
declare -i highmax=100
declare -i guessrange
declare -i computerguess

# Get a random number
(( secretnumber = RANDOM % 100 + 1 ))

# the main bit
while ((userguess != secretnumber));
do

    while :; do
      # let the user input any number they want
      read -p "What is your guess? " userguessunvalidated
      # verify the guess is an integer
      [[ $userguessunvalidated =~ ^[0-9]+$ ]] || { echo "Only whole numbers from 1 to 100 are allowed. Please try again."; continue; }

      # verify guess is between 1 and 100
      if ((userguessunvalidated >= 1 && userguessunvalidated <= 100)); then
        ((totalguesses=totalguesses+1))
        break
      else
        echo "Only whole numbers from 1 to 100 are allowed. Your guess is out of range. Try again. "
      fi
    done

    # assign validated input value to userguess
    userguess=userguessunvalidated

    # some taunts for silly errors in user guesses
    if ((userguess < lowmax)); then
      echo "That guess was lower than a previous guess that was too low. Pay attention!"
    elif ((userguess > highmax)); then
      echo "Wake up! That guess was higher than an earlier guess that was too high."
    fi

    # check for user guesses too high
    if ((userguess > secretnumber)); then
        echo -e "Your guess is too high."
        if ((userguess <= highmax)); then
          ((highmax = userguess-1))
        fi
      fi
      # don't let the computer choose this number or anything higher than this as long as a reasonable user guess was made

    # check for user guesses too low
    if ((userguess < secretnumber)); then
        echo -e "Your guess is too low."
        if ((userguess >= lowmax)); then
          ((lowmax = userguess+1))
        fi
      fi
      # don't let the computer choose this number or anything higher than this as long as a reasonable user guess was made

    # check for user guesses correct
    if [ $userguess == $secretnumber ]; then
        echo -e "*********************************************"
        echo -e "Your guess is correct! Congratulations!"
        echo -e "It took $totalguesses tries."
        echo -e "*********************************************"
        break
      fi

      # Get a random number for the computer between lowmax and highmax, but prevent the computer from trying to generate a random number using a range of 0
      guessrange=highmax-lowmax
      if ((guessrange<=0)); then
        guessrange=1
      fi

      ((computerguess  = RANDOM % guessrange + lowmax ))
      ((totalguesses = totalguesses+1))


      # check for computer guesses too high
      if ((computerguess > secretnumber)); then
          echo -e "The computer guessed $computerguess and that was too high."
          if ((computerguess <= highmax)); then
            ((highmax = computerguess-1))
          fi
        fi

      # check for computer guesses too low
      if ((computerguess < secretnumber)); then
          echo -e "The computer guessed $computerguess and that was too low."
          if ((computerguess >= lowmax)); then
            ((lowmax = computerguess+1))
          fi
        fi

      # check for computer guesses correct
      if [ $computerguess == $secretnumber ]; then
          echo -e "*********************************************"
          echo -e "The computer's guess of $computerguess is correct!"
          echo -e "It took $totalguesses tries."
          echo -e "*********************************************"
          break
        fi

      # these taunts are just for my amusement and to keep the game from being too terribly long
      if (( totalguesses == 8 )); then
          echo -e "Is this a hard number?"
        fi

      if (( totalguesses == 12 )); then
        echo -e "Wow! You are really bad at this."
      fi

      if (( totalguesses >= 16 )); then
          echo -e "This is taking too long."
          echo -e "G A M E   O V E R"
        fi

done

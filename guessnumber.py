#!/usr/bin/env python3

# Guess my number game
#
# --A Python version of a silly game I made on my programmable
#   calculator when I was bored in math class in 1987, with a couple of
#   additions like input validation and computer guesses.
#
# Copyright (c) 2007 Matthew Helmke for the old Python 2 version
# Copyright (c) 2020 Matthew Helmke for the Python 3 version (this one)
#
# To run (Linux):
#   python guessmynumber.py
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

# import random module so that we can generate pseudorandom numbers
import random

# Print a description of the game, with rules, to the screen
print ('''Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!

''')

# Get a random number
secretnumber = random.randrange(100) + 1

# set all our initial values
userguess = 0
totalguesses = 0
lowmax = 0
highmax = 100

# the main bit
while True:

    # let the user input any number they want
    userguessunvalidated = input("What is your guess? ")

    # remove leading and trailing spaces
    userguessunvalidated = str.strip(userguessunvalidated)

    # verify the guess is a number
    if not str.isdigit(userguessunvalidated):
        print ('''Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n''')
        continue
    else:
        userguess = int(userguessunvalidated)

    # make sure the guess is an integer in the right range
    if (userguess <=0 or userguess >=101):
        print ('''Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n''')
        continue

    # some taunts for silly errors in user guesses
    if (userguess < lowmax):
        print ("That guess was lower than a previous guess that was too low. Pay attention!\n")
    if (userguess > highmax):
        print ("Wake up! That guess was higher than an earlier guess that was too high.\n")

    totalguesses += 1

    if (userguess > secretnumber):
        print ("Your guess is too high.\n")
        if (userguess <= highmax):
            highmax = (userguess - 1)
            # don't let the computer choose this number or anything higher than this as long as a reasonable user guess was made

    if (userguess < secretnumber):
        print ("Your guess is too low.\n")
        if (userguess >= lowmax):
            lowmax = (userguess + 1)
            # don't let the computer choose this number or anything lower than this as long as a reasonable user guess was made

    if (userguess == secretnumber):
        print ("\n*********************************************")
        print ("   Your guess is correct! Congratulations!")
        print ("   It took", totalguesses, "total guesses.")
        print ("*********************************************\n")
        input("\n\nPress the enter key to exit.\n")
        break

    # this is to prevent trying to generate a random number from a range of 0
    guessrange = highmax - lowmax
    if (guessrange <= 0):
        guessrange = 1

    # the computer's guess is random, within the range of current reasonable values
    computerguess = (random.randrange(guessrange) + lowmax)
    totalguesses += 1

    if (computerguess > secretnumber):
        print ("The computer guessed", computerguess, "and that was too high.")
        print ("Please try again.\n")
        highmax = (computerguess - 1)

    if (computerguess < secretnumber):
        print ("The computer guessed", computerguess, "and that was too low.")
        print ("Please try again.\n")
        lowmax = (computerguess + 1)

    if (computerguess == secretnumber):
        print ("**********************************************")
        print ("   The computer's guess of", computerguess, "is correct!")
        print ("   It took", totalguesses, "total guesses.")
        print ("**********************************************\n")
        input("\n\nPress the enter key to exit.\n")
        break

    # these taunts are just for my amusement and to keep the game from being too terribly long
    if (totalguesses == 8):
        print ("\nThis is a hard number, isn't it?\n")

    if (totalguesses == 12):
        print ("\nWow! You are really bad at this.\n")

    if (totalguesses >= 16):
        print ("\nYou're taking too long, I can't handle it any more.\n\n")
        print ("G A M E   O V E R\n")
        input("\n\nPress the enter key to exit.\n")
        break

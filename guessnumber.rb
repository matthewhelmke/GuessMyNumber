#!/usr/bin/env ruby

# Guess my number game
#
# --A Ruby version of a silly game I made on my programmable
#   calculator when I was bored in math class in 1987, with a couple of
#   additions like input validation and computer guesses.
#
# Copyright (c) 2007 Matthew Helmke for the old Python 2 version
# Copyright (c) 2026 Matthew Helmke for the Ruby version (this one)
#
# To run:
#   ruby guessnumber.rb
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

puts "Welcome to Guess My Number!"
puts " "
puts "The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible."
puts " "
puts "Here we go!"

# Get a random number

secretnumber = (rand(0-99) + 1)

# set all our initial values

userguessunvalidated = 0
userguess = 1
computerguess = 0
totalguesses = 0
lowmax = 0
highmax = 100
gameover = 0

# the main bit

while gameover == 0

    # let the user input any number they want

    puts " "
    print "What is your guess? "
    userguessunvalidated = gets.chomp.to_i
    totalguesses += 1;

    # perform validation, if passes then pass value to final variable

    if ((userguessunvalidated > 0) && (userguessunvalidated < 101))
            userguess = userguessunvalidated
        else
            puts "Invalid! Please enter a whole number between 1 and 100."
            puts "We will use your previous guess or one if this is your first guess."
    end

    # some taunts for silly errors in user guesses

    if userguess > highmax
        puts " "
        puts "Wake up! That guess was higher than an earlier guess that was too high."
    end
    if userguess < lowmax
        puts " "
        puts "That guess was lower than a previous guess that was too low. Pay attention!"
    end

    # evaluate the user guess

    if userguess == secretnumber
        puts " "
        puts "*********************************************"
        puts "   Your guess is correct! Congratulations!"
        print "   It took ", totalguesses, " total guesses."
        puts " "
        puts "*********************************************"
        gameover = 1
    end

    if userguess > secretnumber
        puts " "
        puts "Your guess is too high."
        highmax = userguess
    end
    if userguess < secretnumber
        puts " "
        puts "Your guess is too low."
        lowmax = userguess
    end

    # the computer's guess is random, within the range of current reasonable values

    computerguess = (rand(lowmax..highmax) + 1)

    # evaluate computer guess

    if computerguess == secretnumber
        puts " "
        puts "**********************************************"
        print "   The computer's guess of ", computerguess, " is correct!"
        puts " "
        print "   It took ", totalguesses, " total guesses."
        puts " "
        puts "**********************************************"
        gameover = 1
    end
    if computerguess > secretnumber
        puts " "
        print "The computer guessed ", computerguess, " and that was too high. "
        puts "Please try again."
        highmax = computerguess
    end
    if computerguess < secretnumber
        puts " "
        print "The computer guessed ", computerguess, " and that was too low. "
        puts "Please try again."
        lowmax = computerguess
    end

    # long game taunts
    if totalguesses == 8
        puts " "
        puts "This is a hard number, isn't it?"
    end
    if totalguesses == 12
        puts " "
        puts "Wow! You are really bad at this."
    end
    if totalguesses == 16
        puts " "
        puts "You're taking too long, I can't handle it any more."
        puts "G A M E   O V E R"
        gameover =1
    end

end

#!/usr/bin/env ruby

# Guess my number game
#
# --A Ruby version of a silly game I made on my programmable
#   calculator when I was bored in math class in 1987, with a couple of
#   additions like input validation and computer guesses.
#
# Copyright (c) 2007 Matthew Helmke for the old Python 2 version
# Copyright (c) 2024 Matthew Helmke for the Ruby version (this one)
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
puts ""

# Get a random number

# A fixed secret from GMN_SECRET for parity testing, otherwise a random one
env_secret = ENV["GMN_SECRET"].to_i
secretnumber = (env_secret >= 1 && env_secret <= 100) ? env_secret : (rand(0-99) + 1)

# set all our initial values

userguessunvalidated = 0
userguess = 1
computerguess = 0
totalguesses = 0
lowmax = 1
highmax = 100

# the main bit

while true
    print "What is your guess? "

    # let the user input any number they want
    line = gets
    break if line.nil?   # end of input
    userguessunvalidated = line.chomp

    # verify the guess is a whole number
    if userguessunvalidated !~ /\A\d+\z/
        puts "Only whole numbers from 1 to 100 are allowed."
        puts "Please try again."
        puts ""
        next
    end

    # verify the guess is in range
    userguess = userguessunvalidated.to_i
    if userguess < 1 || userguess > 100
        puts "Only whole numbers from 1 to 100 are allowed. Your guess is out of range."
        puts "Please try again."
        puts ""
        next
    end

    # this is a real guess, so count it
    totalguesses += 1

    # some taunts for silly errors in user guesses
    if userguess < lowmax
        puts "That guess was lower than a previous guess that was too low. Pay attention!"
        puts ""
    end
    if userguess > highmax
        puts "Wake up! That guess was higher than an earlier guess that was too high."
        puts ""
    end

    # evaluate the guess
    if userguess == secretnumber
        puts ""
        puts "*********************************************"
        puts "   Your guess is correct! Congratulations!"
        puts "   It took #{totalguesses} total guesses."
        puts "*********************************************"
        puts ""
        break
    elsif userguess > secretnumber
        puts "Your guess is too high."
        puts ""
        highmax = userguess - 1 if userguess <= highmax
    else
        puts "Your guess is too low."
        puts ""
        lowmax = userguess + 1 if userguess >= lowmax
    end

    # computer uses the midpoint (binary search) within current bounds
    computerguess = (lowmax + highmax) / 2
    totalguesses += 1

    if computerguess == secretnumber
        puts "**********************************************"
        puts "   The computer's guess of #{computerguess} is correct!"
        puts "   It took #{totalguesses} total guesses."
        puts "**********************************************"
        puts ""
        break
    elsif computerguess > secretnumber
        puts "The computer guessed #{computerguess} and that was too high."
        puts "Please try again."
        puts ""
        highmax = computerguess - 1
    else
        puts "The computer guessed #{computerguess} and that was too low."
        puts "Please try again."
        puts ""
        lowmax = computerguess + 1
    end

    # more taunts and a forced guess limit
    if totalguesses == 8
        puts ""
        puts "This is a hard number, isn't it?"
        puts ""
    elsif totalguesses == 12
        puts ""
        puts "Wow! You are really bad at this."
        puts ""
    elsif totalguesses >= 16
        puts ""
        puts "You're taking too long, I can't handle it any more.\n\nG A M E   O V E R"
        break
    end
end

#!/usr/bin/perl

# Guess my number game
#
# --A Perl version of a silly game I made on my programmable
#   calculator when I was bored in math class in 1987, with a couple of
#   additions like input validation and computer guesses.
#
# Copyright (c) 2007 Matthew Helmke for the old Python 2 version
# Copyright (c) 2020 Matthew Helmke for the Perl version (this one)
#
# Prerequisites:
#   You must have CPAN installed. On most Linux distros, you can install with:
#     cpan App::cpanminus
#     See the CPAN docs for more info: https://www.cpan.org/modules/INSTALL.html
#
#   You must have `cpanminus` installed. On Ubuntu, enter:
#     sudo apt install cpanminus
#
#   You must install the IO::Prompt::Hooked CPAN module like this:
#     sudo cpanm IO::Prompt::Hooked
#
# To run:
#   perl guessnumber.pl
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

use strict;

# set all our initial values
my $totalguesses = 0;
my $lowmax       = 1;
my $highmax      = 100;
my $userguess;
my $computerguess;

# Print a description of the game, with rules, to the screen
print "Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!

";

# Get a random number, or a fixed one from GMN_SECRET for parity testing
my $secretnumber =
    (defined $ENV{GMN_SECRET} && $ENV{GMN_SECRET} =~ /^\d+$/
        && $ENV{GMN_SECRET} >= 1 && $ENV{GMN_SECRET} <= 100)
    ? $ENV{GMN_SECRET}
    : int(rand(100)) + 1;

# the main bit
while (1) {
    print "What is your guess? ";

    # let the user input any number they want
    my $input = <STDIN>;
    last if !defined $input;   # end of input
    chomp($input);
    my $userguessunvalidated = $input;

    # verify the guess is a whole number
    if ($userguessunvalidated !~ /^\d+$/) {
        print "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n\n";
        next;
    }

    # verify the guess is in range
    $userguess = $userguessunvalidated;
    if ($userguess < 1 || $userguess > 100) {
        print "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n\n";
        next;
    }

    # this is a real guess, so count it
    ++$totalguesses;

    # some taunts for silly errors in user guesses
    if ($userguess < $lowmax) {
        print "That guess was lower than a previous guess that was too low. Pay attention!\n\n";
    }
    if ($userguess > $highmax) {
        print "Wake up! That guess was higher than an earlier guess that was too high.\n\n";
    }

    # evaluate the guess
    if ($userguess == $secretnumber) {
        print "\n*********************************************\n";
        print "   Your guess is correct! Congratulations!\n";
        print "   It took $totalguesses total guesses.\n";
        print "*********************************************\n\n";
        exit;
    } elsif ($userguess > $secretnumber) {
        print "Your guess is too high.\n\n";
        $highmax = ($userguess - 1) if $userguess <= $highmax;
    } else {
        print "Your guess is too low.\n\n";
        $lowmax = ($userguess + 1) if $userguess >= $lowmax;
    }

    # computer uses the midpoint (binary search) within current bounds
    $computerguess = int(($lowmax + $highmax) / 2);
    ++$totalguesses;

    if ($computerguess == $secretnumber) {
        print "**********************************************\n";
        print "   The computer's guess of $computerguess is correct!\n";
        print "   It took $totalguesses total guesses.\n";
        print "**********************************************\n\n";
        exit;
    } elsif ($computerguess > $secretnumber) {
        print "The computer guessed $computerguess and that was too high.\nPlease try again.\n\n";
        $highmax = ($computerguess - 1);
    } else {
        print "The computer guessed $computerguess and that was too low.\nPlease try again.\n\n";
        $lowmax = ($computerguess + 1);
    }

    # more taunts and a forced guess limit
    if ($totalguesses == 8) {
        print "\nThis is a hard number, isn't it?\n\n";
    } elsif ($totalguesses == 12) {
        print "\nWow! You are really bad at this.\n\n";
    } elsif ($totalguesses >= 16) {
        print "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n";
        exit;
    }
}

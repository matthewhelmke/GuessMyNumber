<?php

/* Guess my number game

 --A PHP version of a silly game I made on my programmable
   calculator when I was bored in math class in 1987, with a couple of
   additions like input validation and computer guesses.

 Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 Copyright (c) 2020 Matthew Helmke for the PHP version (this one)

 To run:
   php -f guessnumber.php

 This program is free software; you can redistribute it and\or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

// Print a description of the game, with rules, to the screen
echo "Welcome to Guess My Number!

The computer will select a random whole number between 1 and 100.
Your goal is to guess that number. You will get a turn, then a computer
player will get a turn. Each of you are aware of the other's guesses.
The first one to guess the number correctly will win. Try to guess in
as few turns as possible.

Here we go!\n\n";

// Get a random number, or a fixed one from GMN_SECRET for parity testing
$gmn_secret = getenv('GMN_SECRET');
if ($gmn_secret !== false && ctype_digit($gmn_secret) && $gmn_secret >= 1 && $gmn_secret <= 100) {
    $secretnumber = (int)$gmn_secret;
} else {
    $secretnumber = rand(1, 100);
}

// set all our initial values
$userguessunvalidated = 0;
$userguess = 0;
$totalguesses = 0;
$lowmax = 1;
$highmax = 100;
$handle  = fopen('php://stdin', 'r');

// the main bit
while (true) {
    echo "What is your guess? ";

    // let the user input any number they want
    $line = fgets($handle);
    if ($line === false) {
        break; // end of input
    }
    $userguessunvalidated = trim($line);

    // verify the guess is a whole number
    if (!ctype_digit($userguessunvalidated)) {
        echo "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n\n";
        continue;
    }

    // verify the guess is in range
    $userguess = (int)$userguessunvalidated;
    if ($userguess < 1 || $userguess > 100) {
        echo "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n\n";
        continue;
    }

    // this is a real guess, so count it
    $totalguesses++;

    // some taunts for silly errors in user guesses
    if ($userguess < $lowmax) {
        echo "That guess was lower than a previous guess that was too low. Pay attention!\n\n";
    }
    if ($userguess > $highmax) {
        echo "Wake up! That guess was higher than an earlier guess that was too high.\n\n";
    }

    // evaluate the guess
    if ($userguess == $secretnumber) {
        echo "\n*********************************************\n";
        echo "   Your guess is correct! Congratulations!\n";
        echo "   It took $totalguesses total guesses.\n";
        echo "*********************************************\n\n";
        exit;
    } elseif ($userguess > $secretnumber) {
        echo "Your guess is too high.\n\n";
        if ($userguess <= $highmax) {
            $highmax = ($userguess - 1);
        }
    } else {
        echo "Your guess is too low.\n\n";
        if ($userguess >= $lowmax) {
            $lowmax = ($userguess + 1);
        }
    }

    // computer uses the midpoint (binary search) within current bounds
    $computerguess = intval(($lowmax + $highmax) / 2);
    $totalguesses++;

    if ($computerguess == $secretnumber) {
        echo "**********************************************\n";
        echo "   The computer's guess of $computerguess is correct!\n";
        echo "   It took $totalguesses total guesses.\n";
        echo "**********************************************\n\n";
        exit;
    } elseif ($computerguess > $secretnumber) {
        echo "The computer guessed $computerguess and that was too high.\nPlease try again.\n\n";
        $highmax = ($computerguess - 1);
    } else {
        echo "The computer guessed $computerguess and that was too low.\nPlease try again.\n\n";
        $lowmax = ($computerguess + 1);
    }

    // more taunts and a forced guess limit
    if ($totalguesses == 8) {
        echo "\nThis is a hard number, isn't it?\n\n";
    } elseif ($totalguesses == 12) {
        echo "\nWow! You are really bad at this.\n\n";
    } elseif ($totalguesses >= 16) {
        echo "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n";
        exit;
    }
}

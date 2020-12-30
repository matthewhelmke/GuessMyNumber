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
Your goal is to guess that number. You will get a turn, then a
computer player will get a turn. Each of you are\ aware of the
other's guesses. The first one to guess the number correctly will
win. Try to guess in as few turns as possible.

Here we go!\n\n";

// Get a random number
$secretnumber = rand(1,100);

// set all our initial values
$userguessunvalidated = 0;
$userguess = 0;
$totalguesses = 0;
$lowmax = 0;
$highmax = 100;
$won	 = false;
$handle  = fopen('php://stdin', 'r');
$guessrange = 0;

// is writing a function the easiset way to do this?? I don't know
function is_whole_number($var)
{
    return (is_numeric($var)&&(intval($var)==floatval($var)));
}

// the main bit
while (!$won)
{
	echo 'What is your guess? ';

  // let the user input any number they want
	$userguessunvalidated = trim(fgets($handle));

  // if $userguess is a number, continue
	if (preg_match('/\d/', $userguessunvalidated))
	{

    // make sure the guess is an integer
    //if (is_int($userguessunvalidated) == false)
    if (is_whole_number($userguessunvalidated) == false)
    {
        echo "Only whole numbers from 1 to 100 are allowed. Your guess is not a whole number.\nPlease try again.\n";
        continue;
    }

    // make sure the guess is in the right range
    if ($userguessunvalidated <=0 or $userguessunvalidated >=101)
    {
        echo "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n";
        continue;
    }

    $userguess = $userguessunvalidated;

    $totalguesses++;

    // some taunts for silly errors in user guesses
    if ($userguess < $lowmax)
    {
        echo "That guess was lower than a previous guess that was too low. Pay attention!\n";
    }

    if ($userguess > $highmax)
    {
        echo "Wake up! That guess was higher than an earlier guess that was too high.\n";
    }

    // evaluate userguess
    if ($userguess > $secretnumber)
		{
			echo "Your guess is too high.\n";
      if ($userguess <= $highmax)
      {
        $highmax = ($userguess - 1);
      }
		}

		if ($userguess < $secretnumber)
		{
			echo "Your guess is too low.\n";
      if ($userguess >= $lowmax)
      {
        $lowmax = ($userguess + 1);
      }
		}

		if ($userguess == $secretnumber)
		{
			echo "\n*********************************************\n";
      echo "   Your guess is correct! Congratulations!\n";
			printf('    It took %d ' . ngettext('guess', 'guesses', $totalguesses) . "!\n",  $totalguesses);
      echo "*********************************************\n";
			exit;
		}

    // prevent trying to generate a random number from a range of 0
    $guessrange = $highmax - $lowmax;
    if ($guessrange <= 0)
    {
      $guessrange = 1;
    }

    // create a random number for the computer's guess
    $computerguess = (rand(1, ($guessrange - 1)) + $lowmax);

    $totalguesses++;

    // more taunts
    if ($totalguesses == 8)
        echo "\nThis is a hard number, isn't it?\n";

    if ($totalguesses == 12)
        echo "\nWow! You are really bad at this.\n";

    if ($totalguesses >= 16)
    {
        echo "\nYou're taking too long, I can't handle it any more.\n\n";
        echo "G A M E   O V E R\n";
        exit;
    }

    // evaluate computerguess
    if ($computerguess > $secretnumber)
    {
      echo "The computer guessed ", $computerguess, " and that was too high.\n";
      if ($computerguess <= $highmax)
      {
        $highmax = ($computerguess - 1);
      }
    }

    if ($computerguess < $secretnumber)
    {
      echo "The computer guessed ", $computerguess, " and that was too low.\n";
      if ($computerguess >= $lowmax)
      {
        $lowmax = ($computerguess + 1);
      }
    }

    if ($computerguess == $secretnumber)
    {
      echo "\n*********************************************\n";
      echo "   The computer's guess of $computerguess is correct!\n";
      printf('    It took %d ' . ngettext('guess', 'guesses', $totalguesses) . "!\n",  $totalguesses);
      echo "*********************************************\n";
      exit;
    }

	}

}

?>

/* Guess my number game

 --A JavaScript + Node.js version of a silly game I made on my programmable
   calculator when I was bored in math class in 1987, with a couple of
   additions like input validation and computer guesses.

 Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 Copyright (c) 2025 Matthew Helmke for the JavaScript + Node.js version (this one)

 To run:
   You must have Node.js installed
   Run: node guessnumber.js

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


const readline = require("readline");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

function askQuestion(query) {
  return new Promise(resolve => rl.question(query, ans => resolve(ans)));
}

function getRandomComputerTaunt() {
  return computerTaunts[Math.floor(Math.random() * computerTaunts.length)];
}

async function main() {
  // Print a description of the game, with rules, to the screen
  console.log("Welcome to Guess My Number!\n");
  console.log("The computer will select a random whole number between 1 and 100.");
  console.log("Your goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n");
  console.log("Here we go!\n");

  const secretnumber = Math.floor(Math.random() * 100) + 1;
  let userguess = 0;
  let computerguess = 0;
  let totalguesses = 0;
  let lowmax = 1;
  let highmax = 100;

  // Let the user input any number they want, then check if an integer between 1-100
  while (userguess !== secretnumber) {
    let input = await askQuestion("What is your guess? ");

    // Drop anything after a decimal point to make their guess an integer
    userguess = parseInt(input, 10);

    // Check if between 1-100
    if (isNaN(userguess) || userguess < 1 || userguess > 100) {
      console.log("Please enter a valid number between 1 and 100.");
      continue;
    }

    console.log(`You guessed ${userguess}.`);

    totalguesses++;

    // Evaluations and some taunts for silly errors in user guesses
    if (userguess < secretnumber) {
      console.log("Your guess is too low.");
      if (userguess <= lowmax) {
        console.log("That guess was lower than a previous guess that was too low. Pay attention!");
      }
      if (userguess > lowmax) {
        lowmax = userguess;
      }
    } else if (userguess > secretnumber) {
      console.log("Your guess is too high.");
      if (userguess >= highmax) {
        console.log("Wake up! That guess was higher than an earlier guess that was too high.");
      }
      if (userguess < highmax) {
        highmax = userguess;
      }
    } else {
      console.log(`Your guess is correct! Congratulations! It took ${totalguesses} total guesses.\n`);
      return process.exit(1);
    }

    // Time-based taunts
    if (totalguesses === 8) {
      console.log("This is a hard number, isn't it?");
    } else if (totalguesses === 12) {
      console.log("Wow! You are really bad at this.");
    } else if (totalguesses === 16) {
      console.log("You're taking too long, I can't handle it any more. G A M E   O V E R");
      return process.exit(1);
    }

  // Now computer guesses
    computerguess = Math.floor((lowmax + highmax) / 2);
    totalguesses++;

    if (userguess === secretnumber) {
        return;
    } else if (computerguess < secretnumber) {
        console.log(`The computer guessed ${computerguess} and that was too low.`);
        if (computerguess > lowmax) lowmax = computerguess + 1;
    } else if (computerguess > secretnumber) {
        console.log(`The computer guessed ${computerguess} and that was too high.`);
        if (computerguess < highmax) highmax = computerguess - 1;
    } else {
        console.log(`The computer's guess of ${computerguess} is correct! It took ${totalguesses} total guesses!`);
        return process.exit(1);
    }
  }
}

main();

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

// Use readline's async iterator instead of rl.question. With piped stdin the
// readline interface can emit `close` before the next rl.question is queued,
// leaving the Promise unresolved and stalling the game mid-loop. The iterator
// drains buffered lines reliably and signals EOF via { done: true }.
const lineIter = rl[Symbol.asyncIterator]();

async function askQuestion(query) {
  process.stdout.write(query);
  const { value, done } = await lineIter.next();
  return done ? null : value;
}

function getRandomComputerTaunt() {
  return computerTaunts[Math.floor(Math.random() * computerTaunts.length)];
}

async function main() {
  // Print a description of the game, with rules, to the screen
  console.log("Welcome to Guess My Number!\n");
  console.log("The computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n");
  console.log("Here we go!\n");

  // A fixed secret from GMN_SECRET for parity testing, otherwise a random one
  const envSecret = parseInt(process.env.GMN_SECRET, 10);
  const secretnumber = (envSecret >= 1 && envSecret <= 100) ? envSecret : Math.floor(Math.random() * 100) + 1;
  let userguess = 0;
  let computerguess = 0;
  let totalguesses = 0;
  let lowmax = 1;
  let highmax = 100;

  // the main bit
  while (true) {
    let input = await askQuestion("What is your guess? ");
    if (input === null) {
      break; // end of input
    }

    // verify the guess is a whole number
    if (!/^\d+$/.test(input.trim())) {
      console.log("Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n");
      continue;
    }

    // verify the guess is in range
    userguess = parseInt(input, 10);
    if (userguess < 1 || userguess > 100) {
      console.log("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n");
      continue;
    }

    // this is a real guess, so count it
    totalguesses++;

    // some taunts for silly errors in user guesses
    if (userguess < lowmax) {
      console.log("That guess was lower than a previous guess that was too low. Pay attention!\n");
    }
    if (userguess > highmax) {
      console.log("Wake up! That guess was higher than an earlier guess that was too high.\n");
    }

    // evaluate the guess
    if (userguess === secretnumber) {
      console.log(`\n*********************************************\n   Your guess is correct! Congratulations!\n   It took ${totalguesses} total guesses.\n*********************************************\n`);
      break;
    } else if (userguess > secretnumber) {
      console.log("Your guess is too high.\n");
      if (userguess <= highmax) {
        highmax = userguess - 1;
      }
    } else {
      console.log("Your guess is too low.\n");
      if (userguess >= lowmax) {
        lowmax = userguess + 1;
      }
    }

    // computer uses the midpoint (binary search) within current bounds
    computerguess = Math.floor((lowmax + highmax) / 2);
    totalguesses++;

    if (computerguess === secretnumber) {
      console.log(`**********************************************\n   The computer's guess of ${computerguess} is correct!\n   It took ${totalguesses} total guesses.\n**********************************************\n`);
      break;
    } else if (computerguess > secretnumber) {
      console.log(`The computer guessed ${computerguess} and that was too high.\nPlease try again.\n`);
      highmax = computerguess - 1;
    } else {
      console.log(`The computer guessed ${computerguess} and that was too low.\nPlease try again.\n`);
      lowmax = computerguess + 1;
    }

    // more taunts and a forced guess limit
    if (totalguesses === 8) {
      console.log("\nThis is a hard number, isn't it?\n");
    } else if (totalguesses === 12) {
      console.log("\nWow! You are really bad at this.\n");
    } else if (totalguesses >= 16) {
      console.log("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n");
      break;
    }
  }
}

main();

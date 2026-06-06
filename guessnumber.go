// Guess my number game

// --A Golang version of a silly game I made on my programmable
//   calculator when I was bored in math class in 1987, with a couple of
//   additions like input validation and computer guesses.

// Copyright (c) 2007 Matthew Helmke for the old Python 2 version
// Copyright (c) 2023 Matthew Helmke for the Golang version (this one)

//  To run:
//   go run guessnumber.go

// This program is free software; you can redistribute it and\or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"time"
)

func main() {
	lowmax := 1
	highmax := 100

	// Create a local random generator with current time as seed
	rng := rand.New(rand.NewSource(time.Now().UnixNano()))

	// Get a random number, or a fixed one from GMN_SECRET for parity testing
	secretnumber := rng.Intn(highmax-lowmax+1) + lowmax
	if s, err := strconv.Atoi(os.Getenv("GMN_SECRET")); err == nil && s >= 1 && s <= 100 {
		secretnumber = s
	}

	// Print a description of the game, with rules, to the screen
	fmt.Println("Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n\nHere we go!\n\n")

	var userguessunvalidated string

	totalguesses := 0

	// the main bit
	for {
		fmt.Print("What is your guess? ")

		// let the user input any number they want
		if _, err := fmt.Scanf("%s", &userguessunvalidated); err != nil {
			break // end of input
		}

		// verify the guess is a whole number
		userguess, err := strconv.Atoi(userguessunvalidated)
		if err != nil {
			fmt.Println("Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n")
			continue
		}

		// verify the guess is in range
		if userguess < 1 || userguess > 100 {
			fmt.Println("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n")
			continue
		}

		// this is a real guess, so count it
		totalguesses++

		// some taunts for silly errors in user guesses, then evaluate the guess
		if userguess < lowmax {
			fmt.Println("That guess was lower than a previous guess that was too low. Pay attention!\n")
		}
		if userguess > highmax {
			fmt.Println("Wake up! That guess was higher than an earlier guess that was too high.\n")
		}

		if userguess == secretnumber {
			fmt.Printf("\n*********************************************\n   Your guess is correct! Congratulations!\n   It took %d total guesses.\n*********************************************\n\n", totalguesses)
			break
		} else if userguess > secretnumber {
			fmt.Println("Your guess is too high.\n")
			if userguess <= highmax {
				highmax = (userguess - 1)
			}
		} else {
			fmt.Println("Your guess is too low.\n")
			if userguess >= lowmax {
				lowmax = (userguess + 1)
			}
		}

		// computer uses the midpoint (binary search) within current bounds
		computerguess := (lowmax + highmax) / 2
		totalguesses++

		if computerguess == secretnumber {
			fmt.Printf("**********************************************\n   The computer's guess of %d is correct!\n   It took %d total guesses.\n**********************************************\n\n", computerguess, totalguesses)
			break
		} else if computerguess > secretnumber {
			fmt.Printf("The computer guessed %d and that was too high.\nPlease try again.\n\n", computerguess)
			highmax = (computerguess - 1)
		} else {
			fmt.Printf("The computer guessed %d and that was too low.\nPlease try again.\n\n", computerguess)
			lowmax = (computerguess + 1)
		}

		// more taunts and a forced guess limit
		if totalguesses == 8 {
			fmt.Println("\nThis is a hard number, isn't it?\n")
		} else if totalguesses == 12 {
			fmt.Println("\nWow! You are really bad at this.\n")
		} else if totalguesses >= 16 {
			fmt.Println("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n")
			break
		}
	}
}

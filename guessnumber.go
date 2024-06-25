/* Guess my number game
 *
 * --A Golang version of a silly game I made on my programmable
 *   calculator when I was bored in math class in 1987, with a couple of
 *   additions like input validation and computer guesses.
 *
 * Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 * Copyright (c) 2023 Matthew Helmke for the Golang version (this one)
 *
 * To run:
 *   go run guessnumber.go
 *
 * This program is free software; you can redistribute it and\or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package main

import (
	"fmt"
	"math/rand"
	"strconv"
	"time"
)

func main() {
	minNumber := 1
	maxNumber := 100

	// Set the seed value for the random number generator.
	rand.Seed(time.Now().UnixNano())

	// Get a random number
	secretNumber := rand.Intn(maxNumber-minNumber+1) + minNumber

	// Print a description of the game, with rules, to the screen
	fmt.Println("Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\n\nYour goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n\nHere we go!\n\n")
	fmt.Printf("I'm thinking of a number between %d and %d. Can you guess it?\n", minNumber, maxNumber)

	var userguessunvalidated string
	var userguess int
	totalGuesses := 0

	// the main bit
	for {
		fmt.Print("What is your guess? ")

		totalGuesses++

		// let the user input any number they want, then
		// check if an integer
		_, err := fmt.Scanf("%s", &userguessunvalidated)
		userguess, err = strconv.Atoi(userguessunvalidated)

		if err != nil {
			fmt.Println("Invalid! Please enter a whole number between 1 and 100: ")
			continue
		}

		// check if between 1 and 100
		if userguess > 0 && userguess < 101 {

			// some taunts for silly errors in user guesses then evaluate the guess
			if userguess < minNumber {
				fmt.Println("That guess was lower than a previous guess that was too low. Pay attention!\n")
			} else if userguess > maxNumber {
				fmt.Println("Wake up! That guess was higher than an earlier guess that was too high.\n")
			} else if userguess < secretNumber {
				fmt.Println("Your guess is too low.\n")
				minNumber = (userguess + 1)
			} else if userguess > secretNumber {
				fmt.Println("Your guess is too high.\n")
				maxNumber = (userguess - 1)
			} else if userguess == secretNumber {
				fmt.Printf("\n*********************************************\n   Your guess is correct! Congratulations!\n   It took %d total guesses.\n*********************************************\n\n", totalGuesses)
				break
			}
		} else {
			fmt.Println("Invalid! Please enter a whole number between 1 and 100: ")
		}

		// TODO: computer does a random guess, within the range of current reasonable values

		// TODO: evaluate computer guess and print its guess and whether it was high/low

		if totalGuesses == 8 {
			fmt.Println("\nThis is a hard number, isn't it?\n")
		} else if totalGuesses == 12 {
			fmt.Println("\nWow! You are really bad at this.\n")
		} else if totalGuesses == 16 {
			fmt.Printf("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n")
			break
		}
	}
}

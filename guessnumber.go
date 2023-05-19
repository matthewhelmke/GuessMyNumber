/* Guess my number game
 *
 * --A Golang version of a silly game I made on my programmable
 *   calculator when I was bored in math class in 1987, with a couple of
 *   additions like input validation and computer guesses.
 *
 * Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 * Copyright (c) 2020 Matthew Helmke for the C version (this one)
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
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())

    minNumber := 1
    maxNumber := 100
    // Get a random number
    secretNumber := rand.Intn(maxNumber-minNumber+1) + minNumber

    // Print a description of the game, with rules, to the screen
    fmt.Println("Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\n\nYour goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n\nHere we go!\n\n")
    fmt.Printf("I'm thinking of a number between %d and %d. Can you guess it?\n", minNumber, maxNumber)

    var guess int
    totalGuesses := 0

    // the main bit
    for {
        fmt.Print("What is your guess? ")
        // let the user input any number they want, then 
        // TODO: check if an integer between 1-100 - current check does not work
        _, err := fmt.Scanf("%d", &guess)
        if err != nil {
        fmt.Println("Invalid! Please enter a whole number between 1 and 100: ")
        continue
        }

        totalGuesses++
        
        // TODO: some taunts for silly errors in user guesses
        
        
        // evaluate the guess
        if guess < secretNumber {
            fmt.Println("Your guess is too low.\n")
        } else if guess > secretNumber {
            fmt.Println("Your guess is too high.\n")
        } else {
            fmt.Printf("\n*********************************************\n   Your guess is correct! Congratulations!\n   It took %d total guesses.\n*********************************************\n\n", totalGuesses)
        break
        }
        
        //TODO: computer does a random guess, within the range of current reasonable values
        //TODO: evaluate computer guess and pring it's guess and whether it was high/low
    }
}

/* Guess my number game
 *
 * --A Java version of a silly game I made on my programmable
 *   calculator when I was bored in math class in 1987, with a couple of
 *   additions like input validation and computer guesses.
 *
 * Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 * Copyright (c) 2025 Matthew Helmke for the Java version (this one)
 *
 * To compile (Linux):
 *   javac guessnumber.java
 * To run:
 *   java guessnumber
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

import java.util.Scanner;
import java.util.Random;

public class guessnumber {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Random random = new Random();
        int min = 1;
        int max = 100;
        int secretnumber = random.nextInt(max - min + 1) + min; // Get a random number
        int lowmax = 1;
        int highmax = 100;
        int userguess;
        int computerguess;
        int guessrange;
        int totalguesses = 0;
        boolean guessedcorrectly = false;

        // Print a description of the game, with rules, to the screen

        System.out.println("Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\n\nYour goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n\nHere we go!\n\n");

        // let the user input any number they want, then check if an integer between 1-100

        while (!guessedcorrectly) {
            System.out.print("What is your guess? ");
            if (!scanner.hasNextInt()) {
                System.out.println("Please enter a valid number.");
                scanner.next(); // Discard invalid input
                continue;
            }

            userguess = scanner.nextInt();
            totalguesses++;

            // some taunts for silly errors in user guesses
            if (userguess < lowmax) {
                System.out.println("That guess was lower than a previous guess that was too low. Pay attention!\n");
            }
            if (userguess > highmax) {
                System.out.println("TWake up! That guess was higher than an earlier guess that was too high.\n");
            }

            // evaluate the guess
            if (userguess < min || userguess > max) {
                System.out.println("Invalid! Please enter a whole number between 1 and 100: ");
            } else if (userguess < secretnumber) {
                System.out.println("Your guess is too low.\n");
            } else if (userguess > secretnumber) {
                System.out.println("Your guess is too high.\n");
            } else {
                guessedcorrectly = true;
                System.out.println("\n*********************************************\n   Your guess is correct! Congratulations!\n   It took " + totalguesses + "  total guesses.\n*********************************************\n\n");
                System.exit(0);                        
            }
        
            // this is to prevent trying to generate a random number from a range of 0
            guessrange = highmax - lowmax;
            if (guessrange <= 0) {
                guessrange = 1;
            }

            // the computer's guess is random, within the range of current reasonable values
            computerguess = (random.nextInt(guessrange) + lowmax);
            totalguesses++;

            // evaluate the computer guess
            if (computerguess < secretnumber) {
                System.out.println("The computer guessed " + computerguess + " and that was too low.\n\n");
                lowmax = computerguess + 1;
            } else if (computerguess > secretnumber) {
                System.out.println("The computer guessed " + computerguess + " and that was too high.\n\n");
                highmax = computerguess - 1;
            } else {
                guessedcorrectly = true;
                System.out.println("\n*********************************************\n   The computer's guess of " + computerguess + " is correct!\n   It took " + totalguesses + "  total guesses.\n*********************************************\n\n");
                System.exit(0);
            } 
        

            if (totalguesses == 8) {
                System.out.println("\nThis is a hard number, isn't it?\n");
            }

            if (totalguesses == 12) {
                System.out.println("\nWow! You are really bad at this.\n");
            }

            if (totalguesses == 16) {
                System.out.println("\nYou're taking too long, I can't handle it any more.\n\n");
                System.out.println("G A M E   O V E R\n");
            }
        }
    }
}

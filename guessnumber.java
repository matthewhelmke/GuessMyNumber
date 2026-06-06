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
        try (Scanner scanner = new Scanner(System.in)) {
            Random random = new Random();
            int lowmax = 1;
            int highmax = 100;
            // Get a random number, or a fixed one from GMN_SECRET for parity testing
            int secretnumber = random.nextInt(highmax - lowmax + 1) + lowmax;
            String gmnSecret = System.getenv("GMN_SECRET");
            if (gmnSecret != null && gmnSecret.matches("\\d+")
                    && Integer.parseInt(gmnSecret) >= 1 && Integer.parseInt(gmnSecret) <= 100) {
                secretnumber = Integer.parseInt(gmnSecret);
            }
            int userguess;
            int computerguess;
            int totalguesses = 0;

            // Print a description of the game, with rules, to the screen

            System.out.println("Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n\nHere we go!\n\n");

            // the main bit
            while (true) {
                System.out.print("What is your guess? ");

                // let the user input any number they want
                if (!scanner.hasNext()) {
                    break; // end of input
                }
                String userguessunvalidated = scanner.next();

                // verify the guess is a whole number
                if (!userguessunvalidated.matches("\\d+")) {
                    System.out.println("Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n");
                    continue;
                }

                // verify the guess is in range
                userguess = Integer.parseInt(userguessunvalidated);
                if (userguess < 1 || userguess > 100) {
                    System.out.println("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n");
                    continue;
                }

                // this is a real guess, so count it
                totalguesses++;

                // some taunts for silly errors in user guesses
                if (userguess < lowmax) {
                    System.out.println("That guess was lower than a previous guess that was too low. Pay attention!\n");
                }
                if (userguess > highmax) {
                    System.out.println("Wake up! That guess was higher than an earlier guess that was too high.\n");
                }

                // evaluate the guess
                if (userguess == secretnumber) {
                    System.out.println("\n*********************************************\n   Your guess is correct! Congratulations!\n   It took " + totalguesses + " total guesses.\n*********************************************\n");
                    return;
                } else if (userguess > secretnumber) {
                    System.out.println("Your guess is too high.\n");
                    if (userguess <= highmax) {
                        highmax = userguess - 1;
                    }
                } else {
                    System.out.println("Your guess is too low.\n");
                    if (userguess >= lowmax) {
                        lowmax = userguess + 1;
                    }
                }

                // computer uses the midpoint (binary search) within current bounds
                computerguess = (lowmax + highmax) / 2;
                totalguesses++;

                if (computerguess == secretnumber) {
                    System.out.println("**********************************************\n   The computer's guess of " + computerguess + " is correct!\n   It took " + totalguesses + " total guesses.\n**********************************************\n");
                    return;
                } else if (computerguess > secretnumber) {
                    System.out.println("The computer guessed " + computerguess + " and that was too high.\nPlease try again.\n");
                    highmax = computerguess - 1;
                } else {
                    System.out.println("The computer guessed " + computerguess + " and that was too low.\nPlease try again.\n");
                    lowmax = computerguess + 1;
                }

                // more taunts and a forced guess limit
                if (totalguesses == 8) {
                    System.out.println("\nThis is a hard number, isn't it?\n");
                } else if (totalguesses == 12) {
                    System.out.println("\nWow! You are really bad at this.\n");
                } else if (totalguesses >= 16) {
                    System.out.println("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n");
                    return;
                }
            }
        }
    }
}

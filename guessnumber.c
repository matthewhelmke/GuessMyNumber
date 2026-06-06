/* Guess my number game
 *
 * --A Clang version of a silly game I made on my programmable
 *   calculator when I was bored in math class in 1987, with a couple of
 *   additions like input validation and computer guesses.
 *
 * Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 * Copyright (c) 2020 Matthew Helmke for the C version (this one)
 *
 * To compile (Linux):
 *   gcc -o guessnumber guessnumber.c
 * To run:
 *   ./guessnumber
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

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

int
main()
{
        char userguessunvalidated[256];
        int userguess;
        int totalguesses = 0;
        int lowmax = 1;
        int highmax = 100;
        int secretnumber;
        int computerguess;

        // Print a description of the game, with rules, to the screen
        (void) fprintf(stdout,
                       "Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n\nHere we go!\n\n");

        // Get a random number, or a fixed one from GMN_SECRET for parity testing
        char *gmn_secret = getenv("GMN_SECRET");
        if (gmn_secret != NULL && atoi(gmn_secret) >= 1 && atoi(gmn_secret) <= 100) {
                secretnumber = atoi(gmn_secret);
        } else {
                srand(time(0));
                secretnumber = rand() % 100 + 1;
        }

        // the main bit
        for (;;) {

                (void) fprintf(stdout, "What is your guess? ");

                // let the user input any number they want
                if (scanf("%255s", userguessunvalidated) != 1)
                        exit(0); // end of input

                // verify the guess is a whole number (all digits)
                int alldigits = userguessunvalidated[0] != '\0';
                for (int i = 0; userguessunvalidated[i] != '\0'; i++)
                        if (userguessunvalidated[i] < '0' || userguessunvalidated[i] > '9')
                                alldigits = 0;
                if (!alldigits) {
                        (void) fprintf(stdout, "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n\n");
                        continue;
                }

                // verify the guess is in range
                userguess = atoi(userguessunvalidated);
                if (userguess < 1 || userguess > 100) {
                        (void) fprintf(stdout, "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n\n");
                        continue;
                }

                // this is a real guess, so count it
                ++totalguesses;

                // some taunts for silly errors in user guesses
                if (userguess < lowmax)
                        (void) fprintf(stdout, "That guess was lower than a previous guess that was too low. Pay attention!\n\n");
                if (userguess > highmax)
                        (void) fprintf(stdout, "Wake up! That guess was higher than an earlier guess that was too high.\n\n");

                // evaluate the guess
                if (userguess == secretnumber) {
                        (void) fprintf(stdout, "\n*********************************************\n   Your guess is correct! Congratulations!\n   It took %d total guesses.\n*********************************************\n\n", totalguesses);
                        exit(0);
                }

                if (userguess < secretnumber) {
                        (void) fprintf(stdout, "Your guess is too low.\n\n");
                        if (userguess >= lowmax)
                                lowmax = userguess + 1;
                }

                if (userguess > secretnumber) {
                        (void) fprintf(stdout, "Your guess is too high.\n\n");
                        if (userguess <= highmax)
                                highmax = userguess - 1;
                }

                // the computer's guess uses the midpoint of the shared bounds (binary search)
                computerguess = (lowmax + highmax) / 2;
                ++totalguesses;

                if (computerguess == secretnumber) {
                        (void) fprintf(stdout, "**********************************************\n   The computer's guess of %d is correct!\n   It took %d total guesses.\n**********************************************\n\n", computerguess, totalguesses);
                        exit(0);
                }

                if (computerguess < secretnumber) {
                        (void) fprintf(stdout, "The computer guessed %d and that was too low.\nPlease try again.\n\n", computerguess);
                        if (computerguess >= lowmax)
                                lowmax = computerguess + 1;
                }

                if (computerguess > secretnumber) {
                        (void) fprintf(stdout, "The computer guessed %d and that was too high.\nPlease try again.\n\n", computerguess);
                        if (computerguess <= highmax)
                                highmax = computerguess - 1;
                }

                if (totalguesses == 8)
                        (void) fprintf(stdout, "\nThis is a hard number, isn't it?\n\n");

                if (totalguesses == 12)
                        (void) fprintf(stdout, "\nWow! You are really bad at this.\n\n");

                if (totalguesses >= 16) {
                        (void) fprintf(stdout, "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n");
                        exit(0);
                }

        }

        return (0);
}

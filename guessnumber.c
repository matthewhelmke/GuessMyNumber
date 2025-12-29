/* Guess my number game
 *
 * --A Clang version of a silly game I made on my programmable
 *   calculator when I was bored in math class in 1987, with a couple of
 *   additions like input validation and computer guesses.
 *
 * Copyright (c) 2007 Matthew Helmke for the old Python 2 version
 * Copyright (c) 2026 Matthew Helmke for the C version (this one)
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
        char userguessunvalidated;
        char characters;
        int userguess;
        int totalguesses = 0;
        int lowmax = 0;
        int highmax = 100;
        int secretnumber;
        int guessrange;
        int computerguess;
        char buffer[256];
        char name[256];
        FILE *outfile;
        FILE *infile;

        // Print a description of the game, with rules, to the screen
        (void) fprintf(stdout,
                       "Welcome to Guess My Number!\n\nThe computer will select a random whole number between 1 and 100.\n\nYour goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n\nHere we go!\n\n");

        // Get a random number
        srand(time(0));
        secretnumber = rand() % 100 + 1;

        // the main bit
        for (;;) {

                (void) fprintf(stdout, "What is your guess? ");

                // let the user input any number they want, then check if an integer between 1-100
                int c;
                for(;;) {
                        if(((c = scanf("%hhd", &userguessunvalidated)) == 1 || c == EOF) && userguessunvalidated > 0 && userguessunvalidated <101 && ((c = getchar()) == EOF || c == '\n'))
                                break;

                        printf("Invalid! Please enter a whole number between 1 and 100: ");
                        while((c = getchar()) != EOF && c != '\n');
                }

                (userguess=userguessunvalidated);

                ++totalguesses;

                // some taunts for silly errors in user guesses
                if (userguess < lowmax)
                        (void) fprintf(stdout, "That guess was lower than a previous guess that was too low. Pay attention!\n");
                if (userguess > highmax)
                        (void) fprintf(stdout, "Wake up! That guess was higher than an earlier guess that was too high.\n");

                // evaluate the guess
                if (userguess < secretnumber) {
                        (void) fprintf(stdout, "Your guess is too low.\n");
                        if (userguess >= lowmax)
                                (lowmax = userguess + 1);
                }

                if (userguess > secretnumber) {
                        (void) fprintf(stdout, "Your guess is too high.\n");
                        if (userguess <= highmax)
                                (highmax = userguess - 1);
                }

                if (userguess == secretnumber) {
                        (void) fprintf(stdout, "\n*********************************************\n   Your guess is correct! Congratulations!\n   It took %d total guesses.\n*********************************************\n\n", totalguesses);
                        exit(0);
                }

                // this is to prevent trying to generate a random number from a range of 0
                (guessrange = highmax - lowmax);
                if (guessrange <=0)
                        (guessrange = 1);

                // the computer's guess is random, within the range of current reasonable values
                //     computerguess = (random.randrange(guessrange) + lowmax)
                //     totalguesses += 1
                computerguess = (rand() % (guessrange)) + lowmax;
                ++totalguesses;

                if (computerguess < secretnumber) {
                        (void) fprintf(stdout, "The computer guessed %d and that was too low.\n\n", computerguess);
                        if (computerguess >= lowmax)
                                (lowmax = computerguess + 1);
                }

                if (computerguess > secretnumber) {
                        (void) fprintf(stdout, "The computer guessed %d and that was too high.\n\n", computerguess);
                        if (computerguess <= highmax)
                                (highmax = computerguess - 1);
                }

                if (computerguess == secretnumber) {
                        (void) fprintf(stdout, "\n*********************************************\n   The computer's guess of %d is correct!\n", computerguess);
                        (void) fprintf(stdout, "   It took %d total guesses.\n*********************************************\n\n", totalguesses);
                        exit(0);
                }

                if (totalguesses == 8)
                        (void) fprintf(stdout, "\nThis is a hard number, isn't it?\n");

                if (totalguesses == 12)
                        (void) fprintf(stdout, "\nWow! You are really bad at this.\n");

                if (totalguesses == 16) {
                        (void) fprintf(stdout, "\nYou're taking too long, I can't handle it any more.\n\n");
                        (void) fprintf(stdout, "G A M E   O V E R\n");
                        exit(0);
                }

        }

        return (0);
}

/*
 * Guess my number game
 *
 * --A Rust version of a silly game I made on my programmable
 *   calculator when I was bored in math class in 1987, with a couple of
 *   additions like input validation and computer guesses.
 *
 * Original versions:
 *   Copyright (c) 2007 Matthew Helmke
 *   Copyright (c) 2025 Matthew Helmke
 *
 * This Rust version is a direct behavioral port of the Python, Bash, C,
 * Perl, PHP, Racket, COBOL, Go, Ruby, Java, Fortran, and JavaScript versions.
 * I used ChatGPT in the creation of this port, but then edited it further
 * myself.
 *
 * Because Rust is newer and I had to learn, I'm including the following
 * to make it easier for others in the same boat to try this out.
 *
 * To build and run, use Cargo.
 *
 * If you don't have Rust installed, use the official Rust installer:
 *
 * $ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
 *
 * You may need to run `source $HOME/.cargo.env` for the automated changes
 * to your $PATH to take effect.
 *
 * Then, from the directory where this file is located, compile the source
 * code with Cargo:
 *
 * $ cargo new guessnumber
 *
 * This created a new directory to house a new project called `guessnumber`.
 * In the new directory, replace the contents of the `src/main.rs` file with
 * the content of this file. Then, add the `rand` function:
 *
 * $ cargo add rand
 *
 * And run:
 *
 * $ cargo run
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License.
 */

use rand::Rng;
use std::io::{self, Write};
use std::process::exit;

fn read_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

fn main() {
    let mut rng = rand::rng();

    println!("\nWelcome to Guess My Number!\n");
    println!("The computer will select a random whole number between 1 and 100. Your goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n");
    println!("YHere we go!\n");

    let secretnumber = rng.random_range(1..=100);
    
    let mut totalguesses = 0;

    /* Shared bounds based on all prior guesses */
    let mut lowmax = 1;
    let mut highmax = 100;

    loop {
        /* ----- User guess ----- */
        print!("Your guess: ");
        io::stdout().flush().unwrap();

        let guess_input = read_line();
        let userguess: i32 = match guess_input.parse() {
            Ok(n) => n,
            Err(_) => {
                println!("Only whole numbers from 1 to 100 are allowed. Your guess is not a whole number.\nPlease try again.\n");
                continue;
            }
        };

        if user_guess < 1 || user_guess > 100 {
            println!("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n");
            continue;
        }

        totalguesses += 1;

        /* taunts for guesses outside of established limits */
        if userguess < lowmax {
            println!("That guess was lower than a previous guess that was too low. Pay attention!");
        }

        if userguess > highmax {
            println!("Wake up! That guess was higher than an earlier guess that was too high.");
        }

        if userguess == secretnumber {
            println!("\n*********************************************");
            println!("   Your guess of {} is correct!", userguess);
            println!("    It took {} guesses!", totalguesses);
            println!("*********************************************\n");
            break;
        } else if userguess < secretnumber {
            println!("Too low.\n");
            if userguess >= lowmax {
                lowmax = userguess + 1;
            }
        } else {
            println!("Too high.\n");
            if userguess <= highmax {
                highmax = userguess - 1;
            }
        }

        /* PHP-aligned taunt thresholds */
        if totalguesses == 8 {
            println!("\nThis is a hard number, isn't it?\n");
        }

        if totalguesses == 12 {
            println!("\nWow! You are really bad at this.\n");
        }

        if totalguesses >= 16 {
            println!("\nYou're taking too long, I can't handle it any more.\n");
            println!("G A M E   O V E R");
            exit(0);
        }

        /* ----- Computer guess ----- */

        let computerguess = (lowmax + highmax) / 2;
        totalguesses += 1;

        println!("The computer guesses {}.", computerguess);

        if computerguess == secretnumber {
            println!("\n*********************************************");
            println!("   The computer's guess of {} is correct!", computerguess);
            println!("    It took {} guesses!", totalguesses);
            println!("*********************************************\n");
            break;
        } else if computerguess < secretnumber {
            println!("Too low.\n");
            if computerguess >= lowmax {
                lowmax = computerguess + 1;
            }
        } else {
            println!("Too high.\n");
            if computerguess <= highmax {
                highmax = computerguess - 1;
            }
        }

        if total_guesses == 8 {
            println!("\nThis is a hard number, isn't it?\n");
        }

        if total_guesses == 12 {
            println!("\nWow! You are really bad at this.\n");
        }

        if total_guesses >= 16 {
            println!("\nYou're taking too long, I can't handle it any more.\n");
            println!("G A M E   O V E R");
            exit(0);
        }
    }

    println!("Press Enter to exit.");
    let _ = read_line();
}
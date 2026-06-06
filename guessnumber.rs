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
 * the content of this file. Then, run:
 *
 * $ cargo run
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License.
 */

use std::io::{self, Write};
use std::process::exit;
use std::time::{SystemTime, UNIX_EPOCH};

fn read_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}

fn main() {
    println!("Welcome to Guess My Number!\n");
    println!("The computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n");
    println!("Here we go!\n");

    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
    // A fixed secret from GMN_SECRET for parity testing, otherwise a time-based one
    let secretnumber = match std::env::var("GMN_SECRET").ok().and_then(|s| s.parse::<i32>().ok()) {
        Some(n) if (1..=100).contains(&n) => n,
        _ => (now % 100) as i32 + 1,
    };

    let mut totalguesses: i32 = 0;
    let mut lowmax: i32 = 1;
    let mut highmax: i32 = 100;

    loop {
        print!("What is your guess? ");
        io::stdout().flush().unwrap();

        let guess_input = read_line();

        // verify the guess is a whole number
        if guess_input.is_empty() || !guess_input.chars().all(|c| c.is_ascii_digit()) {
            println!("Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n");
            continue;
        }

        // verify the guess is in range
        let userguess: i32 = guess_input.parse().unwrap();
        if userguess < 1 || userguess > 100 {
            println!("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n");
            continue;
        }

        // this is a real guess, so count it
        totalguesses += 1;

        // some taunts for silly errors in user guesses
        if userguess < lowmax {
            println!("That guess was lower than a previous guess that was too low. Pay attention!\n");
        }
        if userguess > highmax {
            println!("Wake up! That guess was higher than an earlier guess that was too high.\n");
        }

        // evaluate the guess
        if userguess == secretnumber {
            println!("\n*********************************************");
            println!("   Your guess is correct! Congratulations!");
            println!("   It took {} total guesses.", totalguesses);
            println!("*********************************************\n");
            break;
        } else if userguess > secretnumber {
            println!("Your guess is too high.\n");
            if userguess <= highmax {
                highmax = userguess - 1;
            }
        } else {
            println!("Your guess is too low.\n");
            if userguess >= lowmax {
                lowmax = userguess + 1;
            }
        }

        // computer uses the midpoint (binary search) within current bounds
        let computerguess = (lowmax + highmax) / 2;
        totalguesses += 1;

        if computerguess == secretnumber {
            println!("**********************************************");
            println!("   The computer's guess of {} is correct!", computerguess);
            println!("   It took {} total guesses.", totalguesses);
            println!("**********************************************\n");
            break;
        } else if computerguess > secretnumber {
            println!("The computer guessed {} and that was too high.\nPlease try again.\n", computerguess);
            highmax = computerguess - 1;
        } else {
            println!("The computer guessed {} and that was too low.\nPlease try again.\n", computerguess);
            lowmax = computerguess + 1;
        }

        // more taunts and a forced guess limit
        if totalguesses == 8 {
            println!("\nThis is a hard number, isn't it?\n");
        } else if totalguesses == 12 {
            println!("\nWow! You are really bad at this.\n");
        } else if totalguesses >= 16 {
            println!("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n");
            exit(0);
        }
    }
}

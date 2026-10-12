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
 *   Input handling and the random secret revised 2026-10-12 with assistance
 *      from Claude Code (claude.ai/code) using the claude-opus-5-5 model
 *
 * This Rust version is a direct behavioral port of the Python, Bash, C,
 * Perl, PHP, Racket, COBOL, Go, Ruby, Java, Fortran, and JavaScript versions.
 * I used ChatGPT in the creation of this port, but then edited it further
 * myself.
 *
 * Because Rust is newer and I had to learn, I'm including the following
 * to make it easier for others in the same boat to try this out.
 *
 * This file uses only Rust's standard library, so rustc, the Rust compiler,
 * can build it directly.
 *
 * If you don't have Rust installed, use the official Rust installer:
 *
 * $ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
 *
 * You may need to run `source $HOME/.cargo/env` for the automated changes
 * to your $PATH to take effect.
 *
 * Then, from the directory where this file is located, compile and run it:
 *
 * $ rustc guessnumber.rs
 * $ ./guessnumber
 *
 * Cargo, Rust's project tool, works too. Create a new project:
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

use std::collections::hash_map::RandomState;
use std::hash::BuildHasher;
use std::io::{self, Write};
use std::process::exit;

fn read_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    // drop the newline, then leading and trailing spaces and tabs; trim() would
    // also remove other whitespace, such as a form feed or a no-break space
    let line = input.strip_suffix('\n').unwrap_or(&input);
    line.trim_matches(|c| c == ' ' || c == '\t').to_string()
}

// A random number from 1 to 100. Rust's standard library has no random number
// generator, but each new RandomState, the type that seeds Rust's hash maps,
// takes fresh keys from the operating system's random source. Hashing any value
// with one gives 64 random bits, and the remainder after dividing by 100 picks
// the secret. 2^64 does not divide evenly by 100, but the bias that leaves is
// about one part in 10^17.
fn random_secret() -> i32 {
    let random_bits: u64 = RandomState::new().hash_one(0u8);
    (random_bits % 100) as i32 + 1
}

fn main() {
    println!("Welcome to Guess My Number!\n");
    println!("The computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n");
    println!("Here we go!\n");

    // A fixed secret from GMN_SECRET for parity testing, otherwise a random one.
    // The value must be all digits, like a guess, because parse accepts a sign.
    let gmn_secret = std::env::var("GMN_SECRET").unwrap_or_default();
    let secretnumber = match gmn_secret.parse::<i32>() {
        Ok(n) if gmn_secret.chars().all(|c| c.is_ascii_digit()) && (1..=100).contains(&n) => n,
        _ => random_secret(),
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

        // verify the guess is in range; the input is all digits, so parse can
        // fail only when the number is too large for an i32, which is out of range
        let userguess: i32 = guess_input.parse().unwrap_or(i32::MAX);
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
        } else if totalguesses == 10 {
            println!("\nWow! You are really bad at this.\n");
        } else if totalguesses >= 12 {
            println!("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n");
            exit(0);
        }
    }
}

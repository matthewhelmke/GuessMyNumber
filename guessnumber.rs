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
 * To build and run, use Cargo or `rustc` for a quick run.
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
    println!("\nWelcome to Guess My Number!\n");
    println!("The computer will select a random whole number between 1 and 100. Your goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n");
    println!("Here we go!\n");

    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
    let secretnumber = (now % 100) as i32 + 1;

    let mut totalguesses: i32 = 0;
    let mut lowmax: i32 = 1;
    let mut highmax: i32 = 100;

    loop {
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

        if userguess < 1 || userguess > 100 {
            println!("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n");
            continue;
        }

        totalguesses += 1;

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
    }

    println!("Press Enter to exit.");
    let _ = read_line();
}

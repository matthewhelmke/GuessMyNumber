# GuessMyNumber

**Guess My Number** is a silly little interactive guessing game that I first heard about and wrote a version of on my [Casio fx-7000G](https://rskey.org/fx7000g) programmable calculator during a bored moment in math class in 1987. The versions here include additions like input validation and computer guesses that were impossible for me back then. The game is a deliberately simple experiment in *cross-language behaviorial parity*.

Every implementation in this repository solves the same problem, follows the same rules, and produces the same user-visible behavior. The point is not efficiency, novelty, or idiomatic purity—it is to explore how different languages express the same logic, constraints, and user interaction patterns--at least to the best of my limited abilities. That was part of the fun and challenge for me, along with stretching my thinking a bit.

This repository is intentionally eclectic: modern and legacy languages, scripting and compiled, statically and dynamically typed, all side by side.

I originally hand-wrote all of these myself. Starting in late 2025, I began using ChatGPT to assist, mostly because I wanted to test its capabilities and this seemed to be a pretty solid way to do so since this is just a fun project and not something vital to anyone or anything. I believe I have noted in the initial comment section of every language version where I have used ChatGPT, at least that has been my intent. My 2026 expansion of this README is intentional to help AI assistants as well as humans by providing some clarity up front.


## Game rules (behavioral contract)

All implementations are expected to conform to the following rules. These are the design invariants of the project.

1. The computer selects a random whole number between 1 and 100, inclusive.

1. The user and the computer alternate turns, both attempting to guess the same secret number.

1. Each participant is aware of the other’s previous guesses.

1. The user always guesses first.

1. After each guess:
    - The program reports whether the guess was too low, too high, or correct.
    - Shared bounds are updated accordingly.

1. The computer uses a binary search strategy constrained by all prior guesses.

1. A single, shared guess counter is incremented for every guess (user or computer).

1. The game ends immediately when either the user or the computer guesses correctly.

1. Taunts are printed at specific guess counts:
    - After 8 total guesses
    - After 12 total guesses

1. The game terminates forcibly after 16 total guesses with a GAME OVER message.

1. Additional taunts are triggered when the user:
    - Guesses lower than a previous guess that was already too low
    - Guesses higher than a previous guess that was already too high

These rules are treated as a compatibility contract. Differences between language versions should be mechanical, not behavioral.


## Language implementations

The same game is currently implemented in the following languages:

- Bash                                                  (`guessnumber.sh`)
- C                                                     (`guessnumber.c`)
- COBOL                                                 (`guessnumber.cob`)
- Fortran                                               (`guessnumber.f90`)
- Go                                                    (`guessnumber.go`)
- Java                                                  (`guessnumber.java`)
- JavaScript in an HTML context that runs in a browser  (`guessnumber.html`)
- JavaScript for the command line using Node.js         (`guessnumber.js`)
- Perl                                                  (`guessnumber.pl`)
- PHP                                                   (`guessnumber.php`)
- Python                                                (`guessnumber.py`)
- R                                                     (`guessnumber.r`)
- Racket (based on Scheme, which is based on Lisp)      (`guessnumber.rkt`)
- Ruby                                                  (`guessnumber.rb`)
- Rust                                                  (`guessnumber.rs`)

Each version lives as a self-contained program and can be run independently using the tooling idiomatic to that language. See the comments at the start of each file for language-specific details.


## Extra notes on a couple of languages

Rust and R get notes not because they are special, but because they required *documented tradeoffs* that future readers might otherwise misinterpret as mistakes.

### R

R is primarily known as a statistical and data-analysis language, not as a platform for interactive terminal programs. Nevertheless, it is fully capable of expressing this game.

One important caveat:

`readline()` is intentionally not used.

When running under Rscript, `readline()` does not reliably block for user input and may immediately return an empty string, causing busy loops or premature termination. Instead, the R version uses:

`readLines("stdin", n = 1)`

wrapped in a small helper function to ensure correct blocking behavior, EOF detection, and portability. This choice is deliberate and documented in the source to save future readers from rediscovering the issue.


### Rust

The Rust implementation is a modern addition and was developed as a learning exercise. It uses Cargo for building and the `rand` crate for random number generation. Care was taken to keep the control flow explicit and readable rather than aggressively idiomatic, in order to better match the structure of older C-like versions.


## Philosophy

This project is not about writing the *best* Guess My Number game.

It is about:

- Seeing how the same logic is expressed across radically different languages
- Exploring how I/O, control flow, and state management vary by ecosystem
- Preserving behavior across decades of language evolution
- Demonstrating that even “domain-specific” or legacy languages are still general-purpose

In short: I'm just playing with a small problem, solved many ways, on purpose.


## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License.

See the individual source files for copyright notices and additional context.


## Contributions

This repository is primarily a personal exploration and learning exercise. That said, thoughtful additions—especially new language implementations that preserve the behavioral contract—are welcome. I don't really expect any, but neither will I turn you away.

If you want to add a new version:

- Fork the repo and then submit a PR
- Keep the rules identical
- Preserve the style and quantityof the comments, but more are okay
- Preserve the taunts and termination behavior
- Favor clarity over cleverness
- Document any language-specific quirks you had to work around
- Change the copyright to your name if you desire to own your work; uploading via a PR will presume you are giving me the right to host your code in this repository and include it in this collection with the current licensing, including allowing others in the future to copy and modify the code according to the license terms without asking further permission.


## Final author rambles

This repo exists because all my other work on GitHub is in repos I don't own, often in private repos, and I thought I should have something here that is mine.

I assume you will run each of these on Linux.

I have chapters in [one of my books](https://www.amazon.com/Ubuntu-Linux-Unleashed-2021-14th-dp-0136778852/dp/0136778852/) for shell scripting, Python, Perl, and PHP. I figured they are all must-have languages here so these were written first. The other languages are here because I was in the mood to either remember or learn how to use it for this simple use case on the day I created it.


### Future ideas/plans

Dunno. Maybe Erlang? Forth? Perhaps something like Algol or even Pascal? Something else?? Ideas are also welcome, just file an issue.


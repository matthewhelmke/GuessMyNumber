# GuessMyNumber

**Guess My Number** is a silly little interactive guessing game that I first heard about and wrote a version of on my [Casio fx-7000G](https://rskey.org/fx7000g) programmable calculator during a bored moment in math class in 1987. The versions here include additions like input validation and computer guesses that were impossible for me back then. The game is a deliberately simple experiment in *cross-language behaviorial parity*.

Every implementation in this repository solves the same problem, follows the same rules, and produces the same user-visible behavior. The point is not efficiency, novelty, or idiomatic purity—it is to explore how different languages express the same logic, constraints, and user interaction patterns--at least to the best of my limited abilities. That was part of the fun and challenge for me, along with stretching my thinking a bit.

This repository is intentionally eclectic: modern and legacy languages, scripting and compiled, statically and dynamically typed, all side by side.

I originally hand-wrote all of these myself.

Starting in *late 2025*, I began working with various AI agents to assist, mostly because I wanted to test its capabilities and this seemed to be a pretty solid way to do so since this is just a fun project and not something vital to anyone or anything. I believe I have noted in the initial comment section of every language version where I have used AI to create that language variant, at least that has been my intent. 

February 15, 2026 was when I first used any AI, specifically GitHub Copilot, which at that moment used `Claude Haiku 4.5`. Together we did some long-deferred maintenance. Since then, AI platforms and models that have helped with any code or content in this repo include:

- [ChatGPT](https://chatgpt.com/)
- [GitHub Copilot](https://github.com/features/copilot) using:
  - `Claude Haiku 4.5`
  - `Grok Code Fast 1`
- [LocalAI](https://localai.io/) using:
  - `gpt-oss-20b`
  - `gpt-oss-120b`
- [Claude Code](https://claude.ai/code) using:
  - `Claude Sonnet 4.6`
  - `Claude Opus 4.7`
  - `Claude Opus 4.8`


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
- BASIC                                                 (`guessnumber.bas`)
- C                                                     (`guessnumber.c`)
- COBOL                                                 (`guessnumber.cob`)
- Erlang using escript                                  (`guessnumber.erl`)
- Forth using Gforth                                    (`guessnumber.fth`)
- Fortran                                               (`guessnumber.f90`)
- Go                                                    (`guessnumber.go`)
- Haskell                                               (`guessnumber.hs`)
- Java                                                  (`guessnumber.java`)
- JavaScript in an HTML context that runs in a browser  (`guessnumber.html`)
- JavaScript for the command line using Node.js         (`guessnumber.js`)
- Lua                                                   (`guessnumber.lua`)
- Pascal                                                (`guessnumber.pas`)
- Perl                                                  (`guessnumber.pl`)
- PHP                                                   (`guessnumber.php`)
- Python                                                (`guessnumber.py`)
- R                                                     (`guessnumber.r`)
- Racket (based on Scheme, which is based on Lisp)      (`guessnumber.rkt`)
- Ruby                                                  (`guessnumber.rb`)
- Rust                                                  (`guessnumber.rs`)

Each version lives as a self-contained program and can be run independently using the tooling idiomatic to that language. See the comments at the start of each file for language-specific details.


## Variable naming convention

All implementations use the same variable names wherever the concept appears, to make side-by-side comparison easier, especially for students.

| Variable | Purpose |
|---|---|
| `secretnumber` | the randomly chosen target number |
| `userguess` | the user's validated guess |
| `userguessunvalidated` | raw user input before validation |
| `totalguesses` | shared counter, incremented for every guess (user or computer) |
| `lowmax` | current lower bound (starts at 1) |
| `highmax` | current upper bound (starts at 100) |
| `guessrange` | `highmax - lowmax` (guards against a zero-range edge case) |
| `computerguess` | the computer's midpoint calculation |

Some languages express these differently — Forth, for example, uses named memory locations rather than typed variables — but the names are preserved regardless.


## Extra notes on a couple of languages

Erlang, Forth, Lua, Pascal, R, and Rust get notes not because they are special, but because they required *documented tradeoffs* that future readers might otherwise misinterpret as mistakes.

### Erlang

Erlang is a functional language built for telecom systems—concurrent, fault-tolerant, and based on immutable data. WhatsApp, RabbitMQ, and CouchDB run on it. This version runs as an **escript**, Erlang's single-file scripting form, so it needs no separate compile step:

`escript guessnumber.erl`

Two things differ from the imperative versions out of necessity, not preference:

- **No mutable variables.** Erlang values cannot be reassigned, so the shared state—the secret number, the guess count, and the bounds—is threaded through a tail-recursive loop. Each turn computes a new state and calls the next function with it. The Haskell version takes the same approach.

- **Reading stdin.** `io:get_line/1` returns the line with its trailing newline, or the atom `eof` when input closes. A small `read_guess/0` helper trims the newline and exits cleanly on `eof`.

### Forth (Gforth)

Standard ANS Forth has no built-in random number facility. The Forth version targets **Gforth** (GNU Forth) and uses two Gforth-specific features not present in all Forth systems:

- `require random.fs` loads Gforth's bundled linear congruential RNG, which provides the `random` word (`n -- 0..n-1`) and a `seed` variable.
- `utime` returns the system clock as microseconds (a double-cell integer), used to seed the RNG at startup.

These are Gforth-specific and not portable to other Forth systems without modification. This is documented in the source and noted here so it is not mistaken for standard ANS Forth. On most Linux systems Gforth is available via the package manager (`apt install gforth` on Debian/Ubuntu derivatives).


### Lua

The Lua version runs under the standard interpreter (`lua guessnumber.lua`; on Debian/Ubuntu derivatives, `apt install lua5.4`). Two details are deliberate:

- **Output uses `io.write`, not `print`.** Lua's `print` separates its arguments with tab characters and appends a newline, which would not match Python's spaces. `io.write` emits exactly the bytes given, so each message is assembled with explicit concatenation and newlines.

- **`goto continue` ends a turn early.** Lua has no `continue` statement, so invalid input jumps to a `::continue::` label at the foot of the loop — the same effect Python gets from `continue`, spelled the way Lua allows.

The `GMN_SECRET` parity hook reads the environment variable with `os.getenv`, matching the other versions.


### Pascal

The Pascal version targets **Free Pascal** (`fpc`) and reads input one line at a time. Two details are deliberate:

- **Parsing reports failure through a code, not an exception.** `Val` converts the trimmed string to an integer and sets a result code. The guess is checked to be all digits first, then `Val` parses it; a non-zero code means the digits overflowed an integer, which the range check then rejects. This matches how Python accepts the value with `int()` and rejects it on range.

- **The loop checks `EOF` before `ReadLn`.** A closed stdin ends the game cleanly instead of looping on empty reads. The prompt prints first, so a piped run that reaches the secret never hits the `EOF` branch.


### R

R is primarily known as a statistical and data-analysis language, not as a platform for interactive terminal programs. Nevertheless, it is fully capable of expressing this game.

One important caveat:

`readline()` is intentionally not used.

When running under Rscript, `readline()` does not reliably block for user input and may immediately return an empty string, causing busy loops or premature termination. Instead, the R version uses:

`readLines("stdin", n = 1)`

wrapped in a small helper function to ensure correct blocking behavior, EOF detection, and portability. This choice is deliberate and documented in the source to save future readers from rediscovering the issue.


### Rust

The Rust implementation is a modern addition and was developed as a learning exercise. It uses Cargo for building. Care was taken to keep the control flow explicit and readable rather than aggressively idiomatic, in order to better match the structure of older C-like versions.


## Philosophy

This project is not about writing the *best* Guess My Number game.

It is about:

- Seeing how the same logic is expressed across radically different languages
- Exploring how I/O, control flow, and state management vary by ecosystem
- Preserving behavior across decades of language evolution
- Demonstrating that even “domain-specific” or legacy languages are still general-purpose

In short: I'm just playing with a small problem, solved many ways, on purpose.


## Test harness

`./tests/run` runs every implementation on a 20-line stdin fixture (`yes 50 | head -n 20`), classifies each as `PASS` / `WARN` / `FAIL` / `TIMEOUT` / `FAIL_BUILD` / `SKIP`, and writes `TEST_REPORT.md` at the repo root.

`./tests/run <lang>` runs a single language without touching the report.

Per-language commands live in `tests/manifest.sh`.

`tests/fixtures/stdin.txt` lists a set of inputs to be used (it's just a list of the number 50 repeated multiple times at the moment).

Locally, `tests/results/` holds log files containing the captured stdout+stderr per run and is gitignored for GitHub. 

BASIC and HTML are skipped for testing as neither has a piped-stdin entry point.

### Parity check

`./tests/run` only confirms that a program terminates correctly; it does not compare output. `./tests/parity` goes further: it runs every command-line implementation under a fixed secret and a fixed input, then diffs the normalized output against the Python version, the canonical reference. Two implementations "agree" only when their output matches line for line.

The fixed secret comes from an environment variable. Every command-line implementation reads `GMN_SECRET` (a number from 1 to 100) as the secret when it is set, and falls back to a random number when it is not. Nothing changes for an ordinary run; the hook exists so the parity check can drive all versions through the same game.

Normalization forgives only the differences a language's print primitives force—trailing whitespace and runs of blank lines. Everything else must match.

This makes `./tests/parity` the gate for a new implementation: add the file, give it the `GMN_SECRET` hook, and fix it until it matches Python. BASIC and HTML are excluded, since neither reads piped stdin; verify those by hand.


## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License.

See the individual source files for copyright notices and additional context.


## Contributions

This repository is primarily a personal exploration and learning exercise. That said, thoughtful additions—especially new language implementations that preserve the behavioral contract—are welcome. I don't really expect any, but neither will I turn you away.

If you want to add a new version:

- Fork the repo and then submit a PR
- Keep the rules identical
- Preserve the style and quantity of the comments; adding more is okay, but don't reduce the number of comments
- Preserve the taunts and termination behavior
- Favor clarity over cleverness
- Document any language-specific quirks you had to work around
- Change the copyright to your name if you desire to own your work; uploading via a PR will presume you are giving me the right to host your code in this repository and include it in this collection with the current licensing, including allowing others in the future to copy and modify the code according to the license terms without asking further permission.


## Final author rambles

This repo exists because all my other work on GitHub is in repos I don't own, often in private repos, and I thought I should have something here that is mine.

I assume you will run each of these on Linux.

I have chapters in [one of my books](https://www.amazon.com/Ubuntu-Linux-Unleashed-2021-14th-dp-0136778852/dp/0136778852/) for shell scripting, Python, Perl, and PHP. I figured they are all must-have languages here so these were written first. The other languages are here because I was in the mood to either remember or learn how to use it for this simple use case on the day I created it.


### Future ideas/plans

Perhaps something like one of these?

- Ada
- Algol
- Common Lisp
- Kotlin
- Prolog
- Zig

Something else?? Ideas are also welcome, just file an issue.


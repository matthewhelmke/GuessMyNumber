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
  - `Claude Opus 5.5`


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
    - After 10 total guesses

1. The game terminates forcibly after 12 total guesses with a GAME OVER message. The computer's binary search always finds the number by its seventh guess, at 14 total guesses, so a limit any later could never be reached; at 12, it ends about a third of the slowest games.

1. Additional taunts are triggered when the user:
    - Guesses lower than a previous guess that was already too low
    - Guesses higher than a previous guess that was already too high

1. Only valid guesses count. A guess is one line of input, ending at a line feed; a carriage return, as a Windows line ending leaves before the line feed, is part of the line. After removing leading and trailing spaces and tabs, and nothing else, a guess is valid when it consists only of the ASCII digits 0 to 9 and its value is from 1 to 100. Any other input gets a message and another try, and does not add to the guess counter:
    - A sign, a decimal point, a space between digits, a letter, a non-ASCII digit such as `²`, or an empty line is not a whole number: "Only whole numbers from 1 to 100 are allowed."
    - A string of digits whose value is below 1 or above 100, however many digits it has, is out of range: the same message, followed by "Your guess is out of range."

These rules are treated as a compatibility contract. Differences between language versions should be mechanical, not behavioral.


## Language implementations

The same game is currently implemented in the following languages:

- Ada using GNAT                                        (`guessnumber.adb`)
- ALGOL 68 using ALGOL 68 Genie                          (`guessnumber.a68`)
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
- Prolog using SWI-Prolog                               (`guessnumber.pro`)
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
| `computerguess` | the computer's midpoint calculation |

Some languages express these differently — Forth, for example, uses named memory locations rather than typed variables, and Erlang and Prolog capitalize them because both treat a capitalized name as a variable — but the names are preserved regardless.


## Extra notes on a couple of languages

Ada, ALGOL 68, Erlang, Forth, Lua, Pascal, Prolog, R, and Rust get notes not because they are special, but because they required *documented tradeoffs* that future readers might otherwise misinterpret as mistakes.

### Ada

Ada was designed for the U.S. Department of Defense and standardized in 1983, and it is named after Ada Lovelace. It is still chosen where software must not fail, such as avionics, rail signaling, and spacecraft. This version is built with **GNAT**, the GNU Ada compiler (`gnatmake guessnumber.adb`, then `./guessnumber`; on Debian/Ubuntu derivatives, `apt install gnat`). Four details are deliberate:

- **The guess is checked to be all digits before `Integer'Value` reads it.** `Integer'Value` also accepts a sign, surrounding spaces, an underscore between digits, and a based literal such as `16#1E#`, which is 30. A run of digits too long for an `Integer` raises `Constraint_Error`, which the program reports as out of range.

- **`Trim` gets a character set.** Given a side such as `Both`, `Ada.Strings.Fixed.Trim` removes only spaces. Given a set holding a space and a tab, it removes both.

- **Ada ignores case.** The shared variable names are written in lower case, as in the other versions, but a variable cannot share a type's name: `guess_status` would clash with the type `Guess_Status`, so the variable is `status`.

- **End of input is an exception.** `Get_Line` raises `End_Error`, and a handler at the end of the procedure ends the game quietly.

Ada has no `continue`, so a `case` statement on the guess's status (not a number, out of range, or valid) does the work of Python's early `continue`.

### ALGOL 68

ALGOL 68 was designed by an international committee and published in 1968. Few people write new programs in it today, but its ideas run through languages they do use: Dennis Ritchie credited it with shaping how C builds its types, and Stephen Bourne, who had worked on an ALGOL 68 compiler, gave his shell `fi` and `esac`. This version runs under **ALGOL 68 Genie** (`a68g guessnumber.a68`; on Debian/Ubuntu derivatives, `apt install algol68g`), which checks the whole program before running it. Five details are deliberate:

- **`AND` evaluates both operands.** Unlike C's `&&`, it does not stop when the left side is false. The `trim` procedure therefore writes each loop condition as an `IF … FI` expression, so it never indexes an empty line. Genie offers a short-circuit `ANDTH`, but it prints a warning on every run.

- **End of input is an event.** A procedure registered with `on logical file end` sets a flag, and the loop checks it right after reading a guess. The flag is cleared before each read, because a last line with no trailing newline still fires the event when the newline is read; without the reset, that final guess would be lost.

- **A form feed is the end of a page, not a character.** Reading a line stops at one, so the guess never contains it. A procedure registered with `on page end` sets a flag instead, and a line that set it is not a whole number.

- **Every run writes a `.Random.seed` file** to the current directory, holding the random generator's state. Genie never reads it back; it seeds itself from the clock, to the second, at startup, as the C version does with `time(0)`. The file is listed in `.gitignore`, since the tests run from the repository root.

- **Integers print with `whole (n, 0)`.** A plain `print` pads a number to a fixed width and adds a sign, which would not match the other versions.

ALGOL 68 has no `continue` or `break`, no `trim`, and no string-to-integer conversion, so the loop ends on a `gameover` flag and the file defines short procedures for the rest. Only `getenv`, which reads the `GMN_SECRET` parity hook, is a Genie extension rather than part of the 1968 language; `a68g --pedantic` flags nothing else.

### Erlang

Erlang is a functional language built for telecom systems—concurrent, fault-tolerant, and based on immutable data. WhatsApp, RabbitMQ, and CouchDB run on it. This version runs as an **escript**, Erlang's single-file scripting form, so it needs no separate compile step:

`escript guessnumber.erl`

Two things differ from the imperative versions out of necessity, not preference:

- **No mutable variables.** Erlang values cannot be reassigned, so the shared state—the secret number, the guess count, and the bounds—is threaded through a tail-recursive loop. Each turn computes a new state and calls the next function with it. The Haskell version takes the same approach.

- **Reading stdin.** A guess is read one character at a time, up to the line feed, with `io:get_chars/2`. `io:get_line/1` turns a carriage return and line feed into a plain line feed, which would make `30` with a Windows line ending a valid guess. `read_guess/0` exits cleanly on `eof`.

### Forth (Gforth)

Standard ANS Forth has no built-in random number facility. The Forth version targets **Gforth** (GNU Forth) and uses two Gforth-specific features not present in all Forth systems:

- `require random.fs` loads Gforth's bundled linear congruential RNG, which provides the `random` word (`n -- 0..n-1`) and a `seed` variable.
- `utime` returns the system clock as microseconds (a double-cell integer), used to seed the RNG at startup.

These are Gforth-specific and not portable to other Forth systems without modification. This is documented in the source and noted here so it is not mistaken for standard ANS Forth. On most Linux systems Gforth is available via the package manager (`apt install gforth` on Debian/Ubuntu derivatives).


### Lua

The Lua version runs under the standard interpreter (`lua guessnumber.lua`; on Debian/Ubuntu derivatives, `apt install lua5.4`). Three details are deliberate:

- **Output uses `io.write`, not `print`.** Lua's `print` separates its arguments with tab characters and appends a newline, which would not match Python's spaces. `io.write` emits exactly the bytes given, so each message is assembled with explicit concatenation and newlines.

- **The user-win banner writes its leading blank line separately.** A long string (`[[ ... ]]`) drops a newline that comes right after its opening brackets, so the blank line the banner starts with is passed to `io.write` as its own `"\n"`.

- **`goto continue` ends a turn early.** Lua has no `continue` statement, so invalid input jumps to a `::continue::` label at the foot of the loop — the same effect Python gets from `continue`, spelled the way Lua allows.

The `GMN_SECRET` parity hook reads the environment variable with `os.getenv`, matching the other versions.


### Pascal

The Pascal version targets **Free Pascal** (`fpc`) and reads input one line at a time. Two details are deliberate:

- **A guess with more than three digits is never converted.** The guess is checked to be all digits, and any guess with more than three digits after its leading zeros is over 100, so it is rejected before conversion. Free Pascal's `Val` reports some overflows through its result code but not all: it read 4294967326, which is 2³² + 30, as 30, and that guess won the game.

- **A guess is read a character at a time.** `ReadLn` also ends a line at a carriage return, so `ReadLineLF` reads up to the line feed with `Read`. The loop checks `EOF` before reading, so a closed stdin ends the game cleanly instead of looping on empty reads. The prompt prints first, so a piped run that reaches the secret never hits the `EOF` branch.


### Prolog

Prolog, created in Marseille in 1972, is a logic programming language: a program states facts and rules, and running it asks the system to prove a goal. It was one of the main languages of 1980s AI research. This version runs under **SWI-Prolog** (`swipl guessnumber.pro`; on Debian/Ubuntu derivatives, `apt install swi-prolog-core`). Four details are deliberate:

- **The file is `guessnumber.pro`,** because `.pl`, Prolog's usual extension, belongs to the Perl version here.

- **No mutable variables.** As in Haskell and Erlang, the game state passes from turn to turn through a tail-recursive loop. Prolog treats a capitalized name as a variable, so the shared names become `Secretnumber`, `Totalguesses`, and so on, as in Erlang.

- **Input is read with the built-in `read_string/5`.** The better-known `read_line_to_string/2` lives in `library(readutil)`, which Debian and Ubuntu split across two packages; with only `swi-prolog-core` installed, loading it prints several errors about a missing shared object before the game starts. At the end of input, `read_string/5` returns an empty string and the separator -1.

- **The guess is checked to be all digits before `number_string/2` reads it,** since that predicate would accept `0x1E` as 30. SWI-Prolog's integers have no size limit, so a very long number is simply out of range.


### R

R is primarily known as a statistical and data-analysis language, not as a platform for interactive terminal programs. Nevertheless, it is fully capable of expressing this game.

One important caveat:

`readline()` is intentionally not used.

When running under Rscript, `readline()` does not reliably block for user input and may immediately return an empty string, causing busy loops or premature termination. Instead, the R version uses:

`readBin` on a binary connection to stdin, one byte at a time up to the line feed,

wrapped in a small helper function to ensure correct blocking behavior and EOF detection. `readLines`, the usual alternative, also ends a line at a carriage return, so `3`, a carriage return, and `0` would read as two guesses. For the same reason the helper trims with `trimws(line, whitespace = "[ \t]")`, since plain `trimws` also removes a carriage return. These choices are deliberate and documented in the source to save future readers from rediscovering the issue.


### Rust

The Rust implementation is a modern addition and was developed as a learning exercise. It uses only the standard library, so it builds with `rustc guessnumber.rs` and needs no Cargo project, though Cargo works too. The standard library has no random number generator, so the secret comes from `RandomState`, the type that seeds Rust's hash maps with keys from the operating system's random source. Care was taken to keep the control flow explicit and readable rather than aggressively idiomatic, in order to better match the structure of older C-like versions.


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

`./tests/run` only confirms that a program terminates correctly; it does not compare output. `./tests/parity` goes further: it plays every command-line implementation through a set of scenarios, then diffs the normalized output of each against the Python version, the canonical reference. Two implementations "agree" only when their output matches line for line.

Each scenario is one game: a fixed secret and a fixed input, listed in `tests/scenarios.sh` with its input in `tests/fixtures/parity/`. Between them, the scenarios reach both winners, a bound of 100, the 8- and 10-guess taunts, the forced game over at 12, both careless-guess taunts, and each kind of invalid input, including the four that trip up a language's own number parser: a hexadecimal literal (`0x1E`), a four-digit number that a three-digit field would truncate to 30 (`1030`), and 2³² + 30 and 2⁶⁴ + 30, which unchecked 32- and 64-bit conversions wrap around to 30. Eleven more test that only spaces and tabs are trimmed and only a line feed ends a line. Six put a carriage return, a vertical tab, a form feed, a no-break space, an ideographic space, or the next-line character U+0085 at one end of a guess. Five put a carriage return, a form feed, U+0085, the line separator U+2028, or the paragraph separator U+2029 inside one, where a reader that ends lines there would see two short guesses instead of one bad one. One scenario ends without a final newline, as piped input can. Each scenario also names text the Python output must contain, such as the GAME OVER message. If a change to the reference or a fixture stops a scenario from reaching its path, the check fails before comparing anything, because that scenario would no longer test what its name says.

When an implementation does not match every scenario yet, `tests/parity_known.txt` lists those pairs, with the cause of each; they report `KNOWN` and do not fail the check. The list only shrinks: a listed pair that starts matching reports `STALE` and fails the check until its line is deleted. `./tests/parity <language>` shows the diffs for a language's known divergences.

The fixed secret comes from an environment variable. Every command-line implementation reads `GMN_SECRET` (a number from 1 to 100) as the secret when it is set, and falls back to a random number when it is not. Nothing changes for an ordinary run; the hook exists so the parity check can drive all versions through the same game.

The hook takes a value exactly when Python does: ASCII digits only, with nothing trimmed, and a value from 1 to 100. The parity check tests this too. Each hook case in `tests/scenarios.sh` sets `GMN_SECRET` to an odd value, such as `+5`, ` 5`, `0x05`, or 2⁶⁴ + 5, and guesses the number a lenient parser would read it as. A win on that first guess means the version took a value Python ignores. An ignored value leaves the secret random, and a random secret matches the guess one time in 100, so each case plays three games aimed at three different numbers (`+5`, `+6`, and `+7`, for example) and decides that the value was taken only when all three win. The games aim at different numbers, rather than repeating one, because some versions seed their random secret from the clock in whole seconds, so games started in the same second share a secret. Python must agree with every case, or the check stops before comparing anything.

Normalization forgives only the differences a language's print primitives force—trailing whitespace and runs of blank lines. Everything else must match.

This makes `./tests/parity` the gate for a new implementation: add the file, give it the `GMN_SECRET` hook, and fix it until it matches Python in every scenario. BASIC and HTML are excluded, since neither reads piped stdin; verify those by hand.


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

- Common Lisp
- Kotlin
- Zig

Something else?? Ideas are also welcome, just file an issue.


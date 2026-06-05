# GuessMyNumber — Claude Code working notes

Local, untracked notes for AI sessions on this repo. Not committed (see `.gitignore`).

## What this repo is

The same guessing game, implemented in many languages. The point is cross-language **behavioral parity**: every version follows the same rules and produces the same user-visible behavior. Differences between versions must be mechanical — forced by a language — never behavioral. The authoritative rules live in `README.md` under "Game rules (behavioral contract)."

## Conventions to uphold

- **Behavioral parity.** Match the contract exactly: a random number from 1 to 100, the user guesses first, a single shared guess counter, a binary-search computer, taunts at 8 and 12 guesses, a forced game over at 16, and the careless-guess taunts.
- **No vestigial or dead code.** Remove anything computed but unused — such as the legacy `guessrange` guard — rather than carry it for the sake of matching other versions.
- **Canonical wording is Python.** `guessnumber.py` is the oldest version and the reference for message text. `guessnumber.hs` (Haskell) is the template for any functional or immutable language: state threaded through a tail-recursive loop instead of mutated.
- **Shared variable names** across every version: `secretnumber`, `userguess`, `userguessunvalidated`, `totalguesses`, `lowmax`, `highmax`, `computerguess`.
- **Attribution.** Each source file carries a GPLv2 header, a per-language copyright line, and a note naming any AI platform and model that helped. Match the existing header style.
- **"Press Enter to exit"** belongs only where a language needs it to keep the terminal open. Omit it everywhere else.

## Testing

- `./tests/run` runs every implementation over a 20-line `50` fixture and writes `TEST_REPORT.md`.
- `./tests/run <lang>` runs one language without rewriting the report.
- Per-language commands live in `tests/manifest.sh`.
- **Caveat:** the harness checks only that a run terminates correctly and exits zero. It does not compare exact output, so it will not catch wording or control-flow drift between versions. Verify parity by hand or by diffing captured output.

## Known parity gaps (open work)

- `guessrange` is computed and clamped but never used. It appears in Bash, C, Forth, Java, Perl, PHP, and Python. Slated for removal everywhere.
- Invalid input counts as a guess in Python (it increments the counter, then continues) but not in Haskell (it recurses with the old count).
- The time-taunts at 8, 12, and 16 are checked once per round in Python (after the computer's move) but twice per round in Haskell (after each half-turn).
- Erlang (`guessnumber.erl`) is already written to the clean target state: no `guessrange`, Python's counting and taunt timing.

## Attribution

Work here is done with Claude Code (Claude Opus 4.8) alongside Matthew. Note the model in a file's header when you add or substantially change it.

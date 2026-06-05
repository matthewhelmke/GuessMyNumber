# Guess My Number — Test Report

Generated 2026-06-05 23:27:44 UTC by `./tests/run`.

Per-language commands live in `tests/manifest.sh`. Stdin fixture is 20
lines of "50" (`tests/fixtures/stdin.txt`). A run **passes** when the
captured output ends with one of "Your guess is correct", "computer …
is correct", or "G A M E   O V E R" **and** the process exits zero.
A **warn** is the same termination pattern with a non-zero exit — typically
an EOF-handling bug that the binary-search convergence masks before stdin
runs out.

## Summary

17 pass · 0 warn · 0 fail · 0 fail-build · 0 timeout · 2 skip · **19 total**

| Language | Status | Duration | Output lines | Notes |
|---|---|---:|---:|---|
| Bash | PASS | 3 ms | 15 |  |
| BASIC | SKIP | — | — | PC-BASIC INPUT statement does not read piped stdin |
| C | PASS | 3 ms | 23 |  |
| COBOL | PASS | 4 ms | 28 |  |
| Erlang | PASS | 104 ms | 50 |  |
| Forth | PASS | 7 ms | 52 |  |
| Fortran | PASS | 4 ms | 45 |  |
| Go | PASS | 29 ms | 27 |  |
| Haskell | PASS | 157 ms | 24 |  |
| HTML | SKIP | — | — | JavaScript-in-browser implementation; no command-line entry point |
| Java | PASS | 49 ms | 47 |  |
| JavaScript | PASS | 20 ms | 29 |  |
| Perl | PASS | 4 ms | 25 |  |
| PHP | PASS | 12 ms | 22 |  |
| Python | PASS | 13 ms | 59 |  |
| R | PASS | 143 ms | 44 |  |
| Racket | PASS | 227 ms | 28 |  |
| Ruby | PASS | 37 ms | 27 |  |
| Rust | PASS | 3 ms | 35 |  |

## Methodology

```bash
printf '%s' "$(cat tests/fixtures/stdin.txt)" | timeout 30s <runner>
```

Per-language stdout+stderr captures are in `tests/results/<lang>.log`
(gitignored).

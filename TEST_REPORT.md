# Guess My Number — Test Report

Generated 2026-06-06 02:55:44 UTC by `./tests/run`.

Per-language commands live in `tests/manifest.sh`. Stdin fixture is 20
lines of "50" (`tests/fixtures/stdin.txt`). A run **passes** when the
captured output ends with one of "Your guess is correct", "computer …
is correct", or "G A M E   O V E R" **and** the process exits zero.
A **warn** is the same termination pattern with a non-zero exit — typically
an EOF-handling bug that the binary-search convergence masks before stdin
runs out.

## Summary

18 pass · 0 warn · 0 fail · 0 fail-build · 0 timeout · 2 skip · **19 total**

| Language | Status | Duration | Output lines | Notes |
|---|---|---:|---:|---|
| Bash | PASS | 3 ms | 49 |  |
| BASIC | SKIP | — | — | PC-BASIC INPUT statement does not read piped stdin |
| C | PASS | 3 ms | 55 |  |
| COBOL | PASS | 4 ms | 38 |  |
| Erlang | PASS | 106 ms | 55 |  |
| Forth | PASS | 5 ms | 38 |  |
| Fortran | PASS | 3 ms | 55 |  |
| Go | PASS | 30 ms | 56 |  |
| Haskell | PASS | 155 ms | 55 |  |
| HTML | SKIP | — | — | JavaScript-in-browser implementation; no command-line entry point |
| Java | PASS | 52 ms | 32 |  |
| JavaScript | PASS | 21 ms | 55 |  |
| Pascal | PASS | 28 ms | 55 |  | 
| Perl | PASS | 4 ms | 38 |  |
| PHP | PASS | 11 ms | 55 |  |
| Python | PASS | 13 ms | 25 |  |
| R | PASS | 140 ms | 55 |  |
| Racket | PASS | 217 ms | 24 |  |
| Ruby | PASS | 36 ms | 48 |  |
| Rust | PASS | 3 ms | 55 |  |

## Methodology

```bash
printf '%s' "$(cat tests/fixtures/stdin.txt)" | timeout 30s <runner>
```

Per-language stdout+stderr captures are in `tests/results/<lang>.log`
(gitignored).

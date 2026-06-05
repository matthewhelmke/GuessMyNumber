# Guess My Number — Test Report

Generated 2026-06-05 22:10:30 UTC by `./tests/run`.

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
| Bash | PASS | 4 ms | 32 |  |
| BASIC | SKIP | — | — | PC-BASIC INPUT statement does not read piped stdin |
| C | PASS | 3 ms | 27 |  |
| COBOL | PASS | 5 ms | 36 |  |
| Erlang | PASS | 113 ms | 50 |  |
| Forth | PASS | 8 ms | 46 |  |
| Fortran | PASS | 6 ms | 45 |  |
| Go | PASS | 2028 ms | 31 |  |
| Haskell | PASS | 744 ms | 30 |  |
| HTML | SKIP | — | — | JavaScript-in-browser implementation; no command-line entry point |
| Java | PASS | 49 ms | 26 |  |
| JavaScript | PASS | 21 ms | 29 |  |
| Perl | PASS | 4 ms | 72 |  |
| PHP | PASS | 15 ms | 22 |  |
| Python | PASS | 17 ms | 52 |  |
| R | PASS | 186 ms | 39 |  |
| Racket | PASS | 251 ms | 28 |  |
| Ruby | PASS | 44 ms | 71 |  |
| Rust | PASS | 3 ms | 30 |  |

## Methodology

```bash
printf '%s' "$(cat tests/fixtures/stdin.txt)" | timeout 30s <runner>
```

Per-language stdout+stderr captures are in `tests/results/<lang>.log`
(gitignored).

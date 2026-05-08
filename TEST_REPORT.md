# Guess My Number — Test Report

Generated 2026-05-08 16:54:18 UTC by `./tests/run`.

Per-language commands live in `tests/manifest.sh`. Stdin fixture is 20
lines of "50" (`tests/fixtures/stdin.txt`). A run **passes** when the
captured output ends with one of "Your guess is correct", "computer …
is correct", or "G A M E   O V E R" **and** the process exits zero.
A **warn** is the same termination pattern with a non-zero exit — typically
an EOF-handling bug that the binary-search convergence masks before stdin
runs out.

## Summary

16 pass · 0 warn · 0 fail · 0 fail-build · 0 timeout · 2 skip · **18 total**

| Language | Status | Duration | Output lines | Notes |
|---|---|---:|---:|---|
| Bash | PASS | 5 ms | 25 |  |
| BASIC | SKIP | — | — | PC-BASIC INPUT statement does not read piped stdin |
| C | PASS | 4 ms | 33 |  |
| COBOL | PASS | 6 ms | 36 |  |
| Forth | PASS | 6 ms | 46 |  |
| Fortran | PASS | 4 ms | 45 |  |
| Go | PASS | 32 ms | 42 |  |
| Haskell | PASS | 169 ms | 45 |  |
| HTML | SKIP | — | — | JavaScript-in-browser implementation; no command-line entry point |
| Java | PASS | 42 ms | 47 |  |
| JavaScript | PASS | 19 ms | 13 |  |
| Perl | PASS | 4 ms | 48 |  |
| PHP | PASS | 11 ms | 30 |  |
| Python | PASS | 14 ms | 59 |  |
| R | PASS | 149 ms | 31 |  |
| Racket | PASS | 223 ms | 26 |  |
| Ruby | PASS | 39 ms | 71 |  |
| Rust | PASS | 3 ms | 35 |  |

## Methodology

```bash
printf '%s' "$(cat tests/fixtures/stdin.txt)" | timeout 30s <runner>
```

Per-language stdout+stderr captures are in `tests/results/<lang>.log`
(gitignored).

# Guess My Number — Test Report

Generated 2026-10-12 20:28:04 UTC by `./tests/run`.

Per-language commands live in `tests/manifest.sh`. Stdin fixture is 20
lines of "50" (`tests/fixtures/stdin.txt`). A run **passes** when the
captured output ends with one of "Your guess is correct", "computer …
is correct", or "G A M E   O V E R" **and** the process exits zero.
A **warn** is the same termination pattern with a non-zero exit — typically
an EOF-handling bug that the binary-search convergence masks before stdin
runs out.

## Summary

22 pass · 0 warn · 0 fail · 0 fail-build · 0 timeout · 2 skip · **24 total**

| Language | Status | Duration | Output lines | Notes |
|---|---|---:|---:|---|
| Ada | PASS | 4 ms | 49 |  |
| ALGOL 68 | PASS | 16 ms | 59 |  |
| Bash | PASS | 3 ms | 25 |  |
| BASIC | SKIP | — | — | PC-BASIC INPUT statement does not read piped stdin |
| C | PASS | 3 ms | 58 |  |
| COBOL | PASS | 5 ms | 58 |  |
| Erlang | PASS | 111 ms | 48 |  |
| Forth | PASS | 6 ms | 48 |  |
| Fortran | PASS | 3 ms | 48 |  |
| Go | PASS | 32 ms | 59 |  |
| Haskell | PASS | 166 ms | 48 |  |
| HTML | SKIP | — | — | JavaScript-in-browser implementation; no command-line entry point |
| Java | PASS | 44 ms | 49 |  |
| JavaScript | PASS | 21 ms | 58 |  |
| Lua | PASS | 4 ms | 39 |  |
| Pascal | PASS | 3 ms | 59 |  |
| Perl | PASS | 4 ms | 38 |  |
| PHP | PASS | 12 ms | 48 |  |
| Prolog | PASS | 14 ms | 25 |  |
| Python | PASS | 15 ms | 18 |  |
| R | PASS | 147 ms | 48 |  |
| Racket | PASS | 245 ms | 38 |  |
| Ruby | PASS | 37 ms | 58 |  |
| Rust | PASS | 4 ms | 58 |  |

## Methodology

```bash
printf '%s' "$(cat tests/fixtures/stdin.txt)" | timeout 30s <runner>
```

Per-language stdout+stderr captures are in `tests/results/<lang>.log`
(gitignored).

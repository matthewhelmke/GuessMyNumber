# Per-language test manifest. Sourced by tests/run.
#
# Arrays:
#   LANGS[k]         — display name (used for the report column and the result file)
#   FILE[k]          — source file (relative to repo root)
#   COMPILE[k]       — optional compile step; %OUT% is replaced with $tmpdir/<k>
#   RUN[k]           — run command; %OUT% is replaced with the compiled binary
#                      (when COMPILE is set) or the source file path
#   STDIN_SUFFIX[k]  — optional extra bytes appended to stdin (e.g. "\n" for
#                      languages that block on a "Press Enter" prompt at exit)
#   SKIP[k]          — if set, language is skipped with this reason
#
# Win pattern is global (see WIN_REGEX in tests/run); override per language only
# if a specific implementation uses unusual termination text.

declare -A LANGS=(
    [bash]="Bash"
    [basic]="BASIC"
    [c]="C"
    [cobol]="COBOL"
    [forth]="Forth"
    [fortran]="Fortran"
    [go]="Go"
    [haskell]="Haskell"
    [html]="HTML"
    [java]="Java"
    [js]="JavaScript"
    [perl]="Perl"
    [php]="PHP"
    [python]="Python"
    [r]="R"
    [racket]="Racket"
    [ruby]="Ruby"
    [rust]="Rust"
)

declare -A FILE=(
    [bash]="guessnumber.sh"
    [basic]="guessnumber.bas"
    [c]="guessnumber.c"
    [cobol]="guessnumber.cob"
    [forth]="guessnumber.fth"
    [fortran]="guessnumber.f90"
    [go]="guessnumber.go"
    [haskell]="guessnumber.hs"
    [html]="guessnumber.html"
    [java]="guessnumber.java"
    [js]="guessnumber.js"
    [perl]="guessnumber.pl"
    [php]="guessnumber.php"
    [python]="guessnumber.py"
    [r]="guessnumber.r"
    [racket]="guessnumber.rkt"
    [ruby]="guessnumber.rb"
    [rust]="guessnumber.rs"
)

declare -A COMPILE=(
    [c]="gcc -o %OUT% guessnumber.c"
    [cobol]="cobc -x -o %OUT% guessnumber.cob"
    [fortran]="gfortran -o %OUT% guessnumber.f90"
    [java]="javac -d %TMPDIR% guessnumber.java"
    [rust]="rustc -o %OUT% guessnumber.rs"
)

declare -A RUN=(
    [bash]="bash guessnumber.sh"
    [c]="%OUT%"
    [cobol]="%OUT%"
    [forth]="gforth guessnumber.fth"
    [fortran]="%OUT%"
    [go]="${GO:-/usr/local/go/bin/go} run guessnumber.go"
    [haskell]="runghc guessnumber.hs"
    [java]="java -cp %TMPDIR% guessnumber"
    [js]="node guessnumber.js"
    [perl]="perl guessnumber.pl"
    [php]="php -f guessnumber.php"
    [python]="python3 guessnumber.py"
    [r]="Rscript guessnumber.r"
    [racket]="racket guessnumber.rkt"
    [ruby]="ruby guessnumber.rb"
    [rust]="%OUT%"
)

# Haskell and R both block on a trailing "Press Enter to exit" — feed one more
# blank line after the convergence input.
declare -A STDIN_SUFFIX=(
    [haskell]=$'\n'
    [r]=$'\n'
)

declare -A SKIP=(
    [basic]="PC-BASIC INPUT statement does not read piped stdin"
    [html]="JavaScript-in-browser implementation; no command-line entry point"
)

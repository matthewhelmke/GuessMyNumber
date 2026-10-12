# Per-language test manifest. Sourced by tests/run.
#
# Arrays:
#   LANGS[k]         — display name (used for the report column and the result file)
#   FILE[k]          — source file (relative to repo root)
#   COMPILE[k]       — optional compile step; %OUT% is replaced with $tmpdir/<k>
#   RUN[k]           — run command; %OUT% is replaced with the compiled binary
#                      (when COMPILE is set) or the source file path
#   SKIP[k]          — if set, language is skipped with this reason
#
# Win pattern is global (see WIN_REGEX in tests/run); override per language only
# if a specific implementation uses unusual termination text.

declare -A LANGS=(
    [ada]="Ada"
    [algol68]="ALGOL 68"
    [bash]="Bash"
    [basic]="BASIC"
    [c]="C"
    [cobol]="COBOL"
    [erlang]="Erlang"
    [forth]="Forth"
    [fortran]="Fortran"
    [go]="Go"
    [haskell]="Haskell"
    [html]="HTML"
    [java]="Java"
    [js]="JavaScript"
    [lua]="Lua"
    [pascal]="Pascal"
    [perl]="Perl"
    [prolog]="Prolog"
    [php]="PHP"
    [python]="Python"
    [r]="R"
    [racket]="Racket"
    [ruby]="Ruby"
    [rust]="Rust"
)

declare -A FILE=(
    [ada]="guessnumber.adb"
    [algol68]="guessnumber.a68"
    [bash]="guessnumber.sh"
    [basic]="guessnumber.bas"
    [c]="guessnumber.c"
    [cobol]="guessnumber.cob"
    [erlang]="guessnumber.erl"
    [forth]="guessnumber.fth"
    [fortran]="guessnumber.f90"
    [go]="guessnumber.go"
    [haskell]="guessnumber.hs"
    [html]="guessnumber.html"
    [java]="guessnumber.java"
    [js]="guessnumber.js"
    [lua]="guessnumber.lua"
    [pascal]="guessnumber.pas"
    [perl]="guessnumber.pl"
    [prolog]="guessnumber.pro"
    [php]="guessnumber.php"
    [python]="guessnumber.py"
    [r]="guessnumber.r"
    [racket]="guessnumber.rkt"
    [ruby]="guessnumber.rb"
    [rust]="guessnumber.rs"
)

declare -A COMPILE=(
    [ada]="gnatmake -q -D %TMPDIR% -o %OUT% guessnumber.adb"
    [c]="gcc -o %OUT% guessnumber.c"
    [cobol]="cobc -x -o %OUT% guessnumber.cob"
    [fortran]="gfortran -o %OUT% guessnumber.f90"
    [java]="javac -d %TMPDIR% guessnumber.java"
    [pascal]="fpc -FE%TMPDIR% -FU%TMPDIR% -opascal guessnumber.pas"
    [rust]="rustc -o %OUT% guessnumber.rs"
)

declare -A RUN=(
    [ada]="%OUT%"
    [algol68]="a68g guessnumber.a68"
    [bash]="bash guessnumber.sh"
    [c]="%OUT%"
    [cobol]="%OUT%"
    [erlang]="escript guessnumber.erl"
    [forth]="gforth guessnumber.fth"
    [fortran]="%OUT%"
    [go]="${GO:-/usr/local/go/bin/go} run guessnumber.go"
    [haskell]="runghc guessnumber.hs"
    [java]="java -cp %TMPDIR% guessnumber"
    [js]="node guessnumber.js"
    [lua]="lua guessnumber.lua"
    [pascal]="%OUT%"
    [perl]="perl guessnumber.pl"
    [prolog]="swipl guessnumber.pro"
    [php]="php -f guessnumber.php"
    [python]="python3 guessnumber.py"
    [r]="Rscript guessnumber.r"
    [racket]="racket guessnumber.rkt"
    [ruby]="ruby guessnumber.rb"
    [rust]="%OUT%"
)

declare -A SKIP=(
    [basic]="PC-BASIC INPUT statement does not read piped stdin"
    [html]="JavaScript-in-browser implementation; no command-line entry point"
)

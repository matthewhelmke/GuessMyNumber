# Parity scenarios. Sourced by tests/parity.
#
# Each scenario is one game: a fixed secret plus the input in
# tests/fixtures/parity/<name>.txt, fed byte for byte. Every line ends in a
# newline, as when a person presses Enter, except in no-final-newline, whose
# only guess has none: piped input can end that way, and the guess still counts.
#
# Arrays:
#   SECRET[name]  — the GMN_SECRET value for the game
#   EXPECT[name]  — extended regular expressions, one per line, that the Python
#                   reference's output must all match. It proves the scenario still reaches the path it
#                   is named for; if the reference stops matching, the scenario
#                   has stopped testing anything, and tests/parity says so.

declare -A SECRET=(
    [original]=42
    [user-win]=50
    [no-final-newline]=50
    [careless-low]=100
    [careless-high]=10
    [bound-at-100]=100
    [trim-spaces]=30
    [trim-tabs]=30
    [empty-line]=30
    [letters]=30
    [sign-minus]=30
    [sign-plus]=30
    [decimal]=30
    [two-numbers]=30
    [zero]=30
    [over-100]=30
    [four-digits]=30
    [huge-number]=30
    [overflow-wrap]=30
    [hex-literal]=30
    [leading-zeros]=8
    [unicode-superscript]=30
    [unicode-arabic-digit]=30
)

NONNUMERIC='Only whole numbers from 1 to 100 are allowed\.$'
OUTOFRANGE='Your guess is out of range\.$'
FIRSTGUESSWIN='It took 1 total guesses\.$'

declare -A EXPECT=(
    [original]="The computer's guess of 42 is correct!"
    [user-win]="$FIRSTGUESSWIN"
    [no-final-newline]="$FIRSTGUESSWIN"
    [careless-low]=$'than a previous guess that was too low\nWow! You are really bad at this\\.\nG A M E   O V E R'
    [careless-high]="Wake up! That guess was higher"
    [bound-at-100]="The computer's guess of 100 is correct!"
    [trim-spaces]="$FIRSTGUESSWIN"
    [trim-tabs]="$FIRSTGUESSWIN"
    [empty-line]="$NONNUMERIC"
    [letters]="$NONNUMERIC"
    [sign-minus]="$NONNUMERIC"
    [sign-plus]="$NONNUMERIC"
    [decimal]="$NONNUMERIC"
    [two-numbers]="$NONNUMERIC"
    [zero]="$OUTOFRANGE"
    [over-100]="$OUTOFRANGE"
    [four-digits]="$OUTOFRANGE"     # 1030 is 30 to a three-digit field that drops high-order digits
    [huge-number]="$OUTOFRANGE"
    [overflow-wrap]="$OUTOFRANGE"   # 2^64 + 30: wraps to 30 in an unchecked 64-bit parse
    [hex-literal]="$NONNUMERIC"     # 0x1E is 30 to a parser that reads source-code literals
    [leading-zeros]="$FIRSTGUESSWIN"
    [unicode-superscript]="$NONNUMERIC"
    [unicode-arabic-digit]="$NONNUMERIC"
)

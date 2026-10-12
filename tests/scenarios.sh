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
    [carriage-return]=30
    [vertical-tab]=30
    [form-feed]=30
    [no-break-space]=30
    [ideographic-space]=30
    [next-line]=30
    [cr-inside]=30
    [form-feed-inside]=30
    [next-line-inside]=30
    [line-separator]=30
    [paragraph-separator]=30
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
    [overflow-32]=30
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
    # Only spaces and tabs are trimmed. These are whitespace to many trim
    # functions, so each one tests that a language's trim removes no more.
    [carriage-return]="$NONNUMERIC"     # 30 then CR, as a Windows line ending leaves it
    [vertical-tab]="$NONNUMERIC"
    [form-feed]="$NONNUMERIC"
    [no-break-space]="$NONNUMERIC"      # U+00A0
    [ideographic-space]="$NONNUMERIC"   # U+3000
    [next-line]="$NONNUMERIC"           # U+0085
    # The same kind of character inside a number. A reader that ends a line at
    # one would see two short guesses, such as 3 and 0, instead of one bad one;
    # at the end of a line that same reader passes by luck, reading an empty line.
    [cr-inside]="$NONNUMERIC"
    [form-feed-inside]="$NONNUMERIC"
    [next-line-inside]="$NONNUMERIC"    # U+0085
    [line-separator]="$NONNUMERIC"      # U+2028
    [paragraph-separator]="$NONNUMERIC" # U+2029
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
    [overflow-32]="$OUTOFRANGE"     # 2^32 + 30: wraps to 30 in an unchecked 32-bit parse
    [overflow-wrap]="$OUTOFRANGE"   # 2^64 + 30: wraps to 30 in an unchecked 64-bit parse
    [hex-literal]="$NONNUMERIC"     # 0x1E is 30 to a parser that reads source-code literals
    [leading-zeros]="$FIRSTGUESSWIN"
    [unicode-superscript]="$NONNUMERIC"
    [unicode-arabic-digit]="$NONNUMERIC"
)

# GMN_SECRET hook cases. The hook takes a value exactly when Python does: ASCII
# digits only, with no trimming, and a value from 1 to 100. Anything else must
# leave the secret random. Each case is "verdict|label|guess:value|..." with
# three guess:value pairs, one per run:
#   takes   — each value must become the secret, so its guess wins at once
#   random  — each value must be ignored; its guess is the number a lax parser
#             would misread it as, so first-guess wins expose one
# The three runs aim at three different numbers because some versions seed
# their random secret from the clock in whole seconds, so runs started in the
# same second share it. One secret cannot equal three different guesses.

HOOK_CASES=(
    "takes|plain|5:5|6:6|7:7"
    "takes|leading-zero|5:05|6:06|7:07"
    "takes|bound-100|98:98|99:99|100:100"
    "random|plus-sign|5:+5|6:+6|7:+7"
    "random|minus-sign|5:-5|6:-6|7:-7"
    "random|leading-space|5: 5|6: 6|7: 7"
    "random|trailing-space|5:5 |6:6 |7:7 "
    $'random|leading-tab|5:\t5|6:\t6|7:\t7'
    "random|trailing-letter|5:5x|6:6x|7:7x"
    "random|decimal|5:5.0|6:6.0|7:7.0"
    "random|hex-literal|5:0x05|6:0x06|7:0x07"
    "random|two-digit-field|5:105|6:106|7:107"              # 105 is 05 to a two-digit field
    "random|three-digit-field|5:1005|6:1006|7:1007"         # 1005 is 005 to a three-digit field
    "random|nine-digit-field|5:1000000005|6:1000000006|7:1000000007"
    "random|overflow-32|5:4294967301|6:4294967302|7:4294967303"     # 2^32 + n
    "random|overflow-wrap|5:18446744073709551621|6:18446744073709551622|7:18446744073709551623"   # 2^64 + n
    "random|superscript|5:⁵|6:⁶|7:⁷"
    "random|arabic-digit|5:٥|6:٦|7:٧"
)

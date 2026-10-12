\ Guess my number game
\
\ -- A Gforth version of a silly game I made on my programmable
\    calculator when I was bored in math class in 1987, with a couple of
\    additions like input validation and computer guesses.
\
\ Copyright (c) 2026 Matthew Helmke for the Gforth version (this one)
\    with assistance from Claude Code (claude.ai/code)
\    using the claude-sonnet-4-6 model
\ Input handling revised 2026-10-12 with assistance from Claude Code
\    (claude.ai/code) using the claude-opus-5-5 model
\
\ To run:
\   gforth guessnumber.fth
\
\ This program is free software; you can redistribute it and/or
\ modify it under the terms of the GNU General Public License
\ as published by the Free Software Foundation; either version 2
\ of the License, or (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.
\
\ You should have received a copy of the GNU General Public License
\ along with this program; if not, write to the Free Software
\ Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\ variables - names match the project standard across all language versions
VARIABLE secretnumber
VARIABLE userguess
VARIABLE totalguesses
VARIABLE lowmax
VARIABLE highmax
VARIABLE computerguess

\ raw input buffer before validation, corresponds to userguessunvalidated in other versions
256 CONSTANT buf-max
CREATE userguessunvalidated buf-max ALLOT
VARIABLE uguess-len

\ random.fs provides the random word; utime seeds it from the system clock (both Gforth extensions)
require random.fs

\ print a description of the game, with rules, to the screen
: print-welcome ( -- )
  ." Welcome to Guess My Number!" cr cr
  ." The computer will select a random whole number between 1 and 100." cr
  ." Your goal is to guess that number. You will get a turn, then a computer" cr
  ." player will get a turn. Each of you are aware of the other's guesses." cr
  ." The first one to guess the number correctly will win. Try to guess in" cr
  ." as few turns as possible." cr cr
  ." Here we go!" cr cr ;

\ true for the two characters a guess may be padded with: space and tab
: blank? ( c -- flag )
  dup bl =  swap 9 =  or ;

\ skip leading spaces and tabs in a string
: skip-spaces ( c-addr u -- c-addr' u' )
  begin
    dup 0 > if over c@ blank? else false then
  while
    1 /string
  repeat ;

\ drop trailing spaces and tabs from a string; the standard -trailing drops only spaces
: skip-trailing-spaces ( c-addr u -- c-addr u' )
  begin
    dup 0 > if 2dup + 1- c@ blank? else false then
  while
    1-
  repeat ;

\ skip leading zeros, keeping the last digit so that "0" stays "0"
: skip-leading-zeros ( c-addr u -- c-addr' u' )
  begin
    dup 1 > if over c@ [char] 0 = else false then
  while
    1 /string
  repeat ;

\ true if the string is not empty and every character is a digit
: all-digits? ( c-addr u -- flag )
  dup 0= if 2drop false exit then
  0. 2swap >number  ( ud c-addr' u' )
  nip nip nip 0= ;

\ convert a string of digits to an integer; returns ( n true ) for a string of
\ digits, ( 0 false ) for anything else. More than three digits after the
\ leading zeros is over 100, so it returns 101 rather than converting a long
\ number, which would wrap around and could land between 1 and 100.
: parse-digits ( c-addr u -- n flag )
  skip-leading-zeros
  2dup all-digits? 0= if 2drop 0 false exit then
  dup 3 > if 2drop 101 true exit then
  0. 2swap >number  ( ud c-addr' u' )
  2drop drop true ;

\ convert a guess to an integer, ignoring leading and trailing spaces and tabs
: parse-integer ( c-addr u -- n flag )
  skip-spaces skip-trailing-spaces parse-digits ;

\ a fixed secret from GMN_SECRET for parity testing, otherwise a random 1..100.
\ The value must be digits only, as a guess must be, but it is not trimmed;
\ an unset variable gives an empty string, which is not digits.
: secret-from-env ( -- n )
  s" GMN_SECRET" getenv parse-digits  ( n flag )
  over 1 101 within and if exit then
  drop 100 random 1 + ;

: init-game ( -- )
  utime drop seed !
  rnd drop                          \ advance once: the LCG's first step from a 64-bit timestamp is low-entropy
  secret-from-env secretnumber !
  0 userguess !
  0 totalguesses !
  1 lowmax !
  100 highmax ! ;

create onechar 1 allot                \ one byte of input at a time

\ read one line into userguessunvalidated, up to a line feed, which is dropped.
\ read-line is not used: it also ends a line at a carriage return, so "3", a
\ carriage return, and "0" would read as two guesses. Neither is accept, which
\ echoes piped input back to stdout. Bytes past buf-max are dropped. Returns
\ false at end of input when nothing was read; a last line with no line feed
\ still counts.
: read-guess-line ( -- flag )
  0 uguess-len !
  begin
    onechar 1 stdin read-file throw  ( u )
    0= if  uguess-len @ 0<> exit  then       \ end of input
    onechar c@ 10 = if  true exit  then       \ line feed: the line is complete
    uguess-len @ buf-max < if
      onechar c@  userguessunvalidated uguess-len @ +  c!
      1 uguess-len +!
    then
  again ;

\ read a line into userguessunvalidated and try to parse it as an integer
: read-user-input ( -- n flag )
  read-guess-line 0= if bye then    \ end of input
  userguessunvalidated uguess-len @ parse-integer ;

\ keep prompting until the user enters a valid whole number between 1 and 100
: get-valid-userguess ( -- n )
  begin
    ." What is your guess? "
    read-user-input
    if  ( n )
      dup 0 > over 101 < and
      if exit then
      drop
      ." Only whole numbers from 1 to 100 are allowed. Your guess is out of range." cr
      ." Please try again." cr cr
    else
      drop
      ." Only whole numbers from 1 to 100 are allowed." cr
      ." Please try again." cr cr
    then
  again ;

\ handle the user's turn; returns true if the game ends
: user-turn ( -- done? )
  get-valid-userguess userguess !
  1 totalguesses +!

  \ some taunts for silly errors in user guesses
  userguess @ lowmax @ < if
    ." That guess was lower than a previous guess that was too low. Pay attention!" cr cr
  then
  userguess @ highmax @ > if
    ." Wake up! That guess was higher than an earlier guess that was too high." cr cr
  then

  \ evaluate the user guess
  userguess @ secretnumber @ > if
    ." Your guess is too high." cr cr
    userguess @ highmax @ <= if userguess @ 1 - highmax ! then
  then
  userguess @ secretnumber @ < if
    ." Your guess is too low." cr cr
    userguess @ lowmax @ >= if userguess @ 1 + lowmax ! then
  then
  userguess @ secretnumber @ = if
    cr
    ." *********************************************" cr
    ."    Your guess is correct! Congratulations!" cr
    ."    It took " totalguesses @ . ." total guesses." cr
    ." *********************************************" cr cr
    true exit
  then
  false ;

\ handle the computer's turn; returns true if the game ends
: computer-turn ( -- done? )
  \ the computer's guess uses a binary-search midpoint within current bounds
  lowmax @ highmax @ + 2 / computerguess !
  1 totalguesses +!

  computerguess @ secretnumber @ > if
    ." The computer guessed " computerguess @ . ." and that was too high." cr
    ." Please try again." cr cr
    computerguess @ highmax @ <= if computerguess @ 1 - highmax ! then
  then
  computerguess @ secretnumber @ < if
    ." The computer guessed " computerguess @ . ." and that was too low." cr
    ." Please try again." cr cr
    computerguess @ lowmax @ >= if computerguess @ 1 + lowmax ! then
  then
  computerguess @ secretnumber @ = if
    ." **********************************************" cr
    ."    The computer's guess of " computerguess @ . ." is correct!" cr
    ."    It took " totalguesses @ . ." total guesses." cr
    ." **********************************************" cr cr
    true exit
  then
  false ;

\ these taunts are just for amusement and to keep the game from being too terribly long
: check-taunts ( -- )
  totalguesses @ 8  = if cr ." This is a hard number, isn't it?" cr cr then
  totalguesses @ 10 = if cr ." Wow! You are really bad at this." cr cr then ;

: check-game-over ( -- done? )
  totalguesses @ 12 >= if
    cr ." You're taking too long, I can't handle it any more." cr cr
    ." G A M E   O V E R" cr cr
    true exit
  then
  false ;

\ the main bit
: play-game ( -- )
  init-game
  print-welcome
  begin
    user-turn     if exit then
    computer-turn if exit then
    check-taunts
    check-game-over if exit then
  again ;

play-game
bye

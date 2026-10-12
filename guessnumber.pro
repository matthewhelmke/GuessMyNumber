/*
  Guess my number game

  A Prolog version of a silly game I made on my programmable
  calculator when I was bored in math class in 1987, with a couple of
  additions like input validation and computer guesses.

  Copyright (c) 2026 Matthew Helmke for the Prolog version (this one)
     with assistance from Claude Code (claude.ai/code)
     using the claude-opus-5-5 model

  This Prolog version is a direct behavioral port of the canonical Python
  version, shaped like the Haskell and Erlang versions.

  To run (SWI-Prolog):
    swipl guessnumber.pro

  Language notes (Prolog choices a reader should not mistake for bugs):

    * The file is named .pro, not the usual .pl, because guessnumber.pl is
      the Perl version. SWI-Prolog runs a file of either name.

    * A Prolog program is a set of facts and rules, not a list of steps.
      Each predicate below reads as "this is true when ...", and running the
      program asks SWI-Prolog to prove main. The initialization directive at
      the top names main as the goal, and SWI-Prolog exits when it is proved.

    * Prolog variables cannot be changed once they have a value, so the
      game state -- the secret number, the guess count, and the bounds -- is
      passed from one turn to the next through a tail-recursive loop, as in
      the Haskell and Erlang versions. Totalguesses1 is the count after the
      next guess, where Haskell writes totalguesses'.

    * A name starting with a capital letter is a variable, and one starting
      with a lower-case letter is a constant. The shared variable names are
      therefore written Secretnumber, Userguess, and so on, as in Erlang.

    * ( If -> Then ; Else ) is Prolog's if-then-else, and "is" evaluates
      arithmetic: Lowmax1 is Userguess + 1. A plain = would only match the
      two sides as patterns, without computing anything.

    * 0'0 and 0'9 are the character codes of the digits 0 and 9.

    * number_string/2 would accept far more than a guess should, including
      0x1E as 30 and 1_000 as 1000, so the guess is checked to be all digits
      first. SWI-Prolog's integers have no size limit, so a very long
      number stays a very long number and the range check rejects it.

    * The input is read with the built-in read_string/5, not
      read_line_to_string/2, which lives in library(readutil). Debian and
      Ubuntu split that library across two packages, so on a system with
      only swi-prolog-core installed, loading it prints several errors
      about a missing shared object before the game starts. At the end of
      the input, read_string/5 returns an empty string and the separator
      -1, which ends the game quietly. A last line with no newline also
      returns -1, but with the guess in it, so the guess still counts.

    * SWI-Prolog prints a |: prompt before reading from a terminal, but only
      when the cursor is at the start of a line. "What is your guess? " always
      leaves it mid-line, so the prompt never appears.

    * random_between/3 draws from a generator that SWI-Prolog seeds afresh
      for each run, so each game picks a new secret number.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

:- initialization(main, main).

% Entry point: print the rules, choose the secret number, and start the game.
main :-
    % Print a description of the game, with rules, to the screen
    format("Welcome to Guess My Number!~n~n"),
    format("The computer will select a random whole number between 1 and 100.~n"),
    format("Your goal is to guess that number. You will get a turn, then a computer~n"),
    format("player will get a turn. Each of you are aware of the other's guesses.~n"),
    format("The first one to guess the number correctly will win. Try to guess in~n"),
    format("as few turns as possible.~n~n"),
    format("Here we go!~n~n~n"),

    secret_from_env(Secretnumber),

    % Start with no guesses yet and shared bounds: Lowmax = 1, Highmax = 100
    game_loop(Secretnumber, 0, 1, 100).

% A fixed secret from GMN_SECRET for parity testing, otherwise a random one.
% getenv/2 returns an atom, the type for constants, so it becomes a string.
secret_from_env(Secretnumber) :-
    (   getenv('GMN_SECRET', Atom),
        atom_string(Atom, Text),
        parse_guess(Text, valid(Value))
    ->  Secretnumber = Value
    ;   random_between(1, 100, Secretnumber)
    ).

% The main bit: read a guess, check it, and act on what it turned out to be.
% Totalguesses is the shared counter; a rejected input does not add to it.
game_loop(Secretnumber, Totalguesses, Lowmax, Highmax) :-
    format("What is your guess? "),
    % let the user input any number they want
    read_guess(Userguessunvalidated),
    % remove leading and trailing spaces and tabs
    split_string(Userguessunvalidated, "", " \t", [Trimmed]),
    parse_guess(Trimmed, Result),
    take_turn(Result, Secretnumber, Totalguesses, Lowmax, Highmax).

% Read one line of input, without its newline. Stop the game at end of input.
read_guess(Userguessunvalidated) :-
    read_string(user_input, "\n", "", Separator, Line),
    (   Separator == -1,
        Line == ""
    ->  halt
    ;   Userguessunvalidated = Line
    ).

% Sort a guess into not_a_number, out_of_range, or valid(Number).
parse_guess(Text, Result) :-
    (   all_digits(Text)
    ->  number_string(Number, Text),
        (   between(1, 100, Number)
        ->  Result = valid(Number)
        ;   Result = out_of_range
        )
    ;   Result = not_a_number
    ).

% True only for a non-empty string of ASCII digits.
all_digits(Text) :-
    string_codes(Text, Codes),
    Codes \== [],
    forall(member(Code, Codes), between(0'0, 0'9, Code)).

% Act on a guess: re-prompt after bad input, or play the turn.
take_turn(not_a_number, Secretnumber, Totalguesses, Lowmax, Highmax) :-
    format("Only whole numbers from 1 to 100 are allowed.~n"),
    format("Please try again.~n~n"),
    game_loop(Secretnumber, Totalguesses, Lowmax, Highmax).
take_turn(out_of_range, Secretnumber, Totalguesses, Lowmax, Highmax) :-
    format("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.~n"),
    format("Please try again.~n~n"),
    game_loop(Secretnumber, Totalguesses, Lowmax, Highmax).
take_turn(valid(Userguess), Secretnumber, Totalguesses, Lowmax, Highmax) :-
    % this is a real guess, so count it
    Totalguesses1 is Totalguesses + 1,
    user_turn(Secretnumber, Totalguesses1, Lowmax, Highmax, Userguess).

% The user's turn: scold a careless guess, then evaluate it. A correct guess
% ends the game. Otherwise narrow the bounds and pass play to the computer.
user_turn(Secretnumber, Totalguesses, Lowmax, Highmax, Userguess) :-
    % some taunts for silly errors in user guesses
    (   Userguess < Lowmax
    ->  format("That guess was lower than a previous guess that was too low. Pay attention!~n~n")
    ;   true
    ),
    (   Userguess > Highmax
    ->  format("Wake up! That guess was higher than an earlier guess that was too high.~n~n")
    ;   true
    ),

    % evaluate the guess
    (   Userguess =:= Secretnumber
    ->  format("~n*********************************************~n"),
        format("   Your guess is correct! Congratulations!~n"),
        format("   It took ~d total guesses.~n", [Totalguesses]),
        format("*********************************************~n~n")
    ;   Userguess > Secretnumber
    ->  format("Your guess is too high.~n~n"),
        % move the upper bound down, but never past a tighter existing bound
        Highmax1 is min(Highmax, Userguess - 1),
        computer_turn(Secretnumber, Totalguesses, Lowmax, Highmax1)
    ;   format("Your guess is too low.~n~n"),
        % move the lower bound up, but never past a tighter existing bound
        Lowmax1 is max(Lowmax, Userguess + 1),
        computer_turn(Secretnumber, Totalguesses, Lowmax1, Highmax)
    ).

% The computer's turn: guess the midpoint of the current bounds. A correct
% guess ends the game. Otherwise narrow the bounds and finish the round.
computer_turn(Secretnumber, Totalguesses, Lowmax, Highmax) :-
    Computerguess is (Lowmax + Highmax) // 2,
    Totalguesses1 is Totalguesses + 1,
    (   Computerguess =:= Secretnumber
    ->  format("**********************************************~n"),
        format("   The computer's guess of ~d is correct!~n", [Computerguess]),
        format("   It took ~d total guesses.~n", [Totalguesses1]),
        format("**********************************************~n~n")
    ;   Computerguess > Secretnumber
    ->  format("The computer guessed ~d and that was too high.~n", [Computerguess]),
        format("Please try again.~n~n"),
        Highmax1 is Computerguess - 1,
        after_round(Secretnumber, Totalguesses1, Lowmax, Highmax1)
    ;   format("The computer guessed ~d and that was too low.~n", [Computerguess]),
        format("Please try again.~n~n"),
        Lowmax1 is Computerguess + 1,
        after_round(Secretnumber, Totalguesses1, Lowmax1, Highmax)
    ).

% These taunts are just for my amusement and to keep the game from being too
% terribly long. Loop for another round unless the guess limit ends the game.
after_round(Secretnumber, Totalguesses, Lowmax, Highmax) :-
    (   Totalguesses =:= 8
    ->  format("~nThis is a hard number, isn't it?~n~n")
    ;   true
    ),
    (   Totalguesses =:= 10
    ->  format("~nWow! You are really bad at this.~n~n")
    ;   true
    ),
    (   Totalguesses >= 12
    ->  format("~nYou're taking too long, I can't handle it any more.~n~nG A M E   O V E R~n~n")
    ;   game_loop(Secretnumber, Totalguesses, Lowmax, Highmax)
    ).

#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Guess My Number - Erlang
%%
%% --An Erlang version of a silly game I made on my programmable
%%   calculator when I was bored in math class in 1987, with a couple of
%%   additions like input validation and computer guesses.
%%
%% Copyright (c) 2026 Matthew Helmke for the Erlang version (this one)
%%
%% This Erlang version is a direct behavioral port of the Python and Haskell
%% versions. I built it with Claude Code using Claude Opus 4.8, then reviewed
%% it myself.
%%
%% To run:
%%   escript guessnumber.erl
%%
%% Language notes (Erlang choices a reader should not mistake for bugs):
%%
%%   * Erlang values are immutable, so the shared game state -- the secret
%%     number, the running guess count, and the lowmax/highmax bounds -- is
%%     threaded through a tail-recursive loop instead of being changed in
%%     place. This mirrors the Haskell version. There is no `for` loop and
%%     there are no reassigned variables; each turn computes a new state and
%%     calls the next function with it.
%%
%%   * io:get_line/1 returns the input line WITH its trailing newline, or the
%%     atom 'eof' when the input stream closes (for example, when piped stdin
%%     runs dry). read_guess/0 trims the newline and halts cleanly on eof.
%%
%%   * The computer's guess is simply the midpoint of the current bounds. The
%%     bounds always enclose the secret number, so the range never collapses and
%%     needs no special-case guard.
%%
%%   * rand:uniform/1 auto-seeds the process from system entropy on first use,
%%     so each run draws a fresh secret number with no explicit seeding.
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

%% Entry point. escript calls main/1 with the command-line arguments.
main(_Args) ->
    %% Print a description of the game, with rules, to the screen.
    io:format(
      "Welcome to Guess My Number!~n~n"
      "The computer will select a random whole number between 1 and 100.~n"
      "Your goal is to guess that number. You will get a turn, then a computer~n"
      "player will get a turn. Each of you are aware of the other's guesses.~n"
      "The first one to guess the number correctly will win. Try to guess in~n"
      "as few turns as possible.~n~n"
      "Here we go!~n~n"),

    %% Draw the secret number: a fixed one from GMN_SECRET for parity testing,
    %% otherwise a random 1..100 (rand auto-seeds the process).
    Secretnumber = secret_from_env(),

    %% Start with shared bounds: lowmax = 1, highmax = 100, no guesses yet.
    game_loop(Secretnumber, 0, 1, 100).

%% The main bit: read and validate the user's guess, then hand off to the turn
%% logic. Totalguesses is the shared counter; it advances only for a guess we
%% actually evaluate, so a rejected input does not consume a turn.
game_loop(Secretnumber, Totalguesses, Lowmax, Highmax) ->
    Userguessunvalidated = read_guess(),
    case parse_guess(Userguessunvalidated) of
        not_a_number ->
            io:format("Only whole numbers from 1 to 100 are allowed.~n"
                      "Please try again.~n~n"),
            game_loop(Secretnumber, Totalguesses, Lowmax, Highmax);
        out_of_range ->
            io:format("Only whole numbers from 1 to 100 are allowed. "
                      "Your guess is out of range.~n"
                      "Please try again.~n~n"),
            game_loop(Secretnumber, Totalguesses, Lowmax, Highmax);
        {ok, Userguess} ->
            user_turn(Secretnumber, Totalguesses + 1, Lowmax, Highmax, Userguess)
    end.

%% The user's turn: scold a careless guess, then evaluate it. A correct guess
%% ends the game (the function simply returns). Otherwise we narrow the bounds
%% and pass play to the computer.
user_turn(Secretnumber, Totalguesses, Lowmax, Highmax, Userguess) ->
    %% Some taunts for silly errors in user guesses, based on shared bounds.
    maybe_careless_low(Userguess, Lowmax),
    maybe_careless_high(Userguess, Highmax),

    %% Evaluate the guess.
    if
        Userguess =:= Secretnumber ->
            io:format("~n*********************************************~n"
                      "   Your guess is correct! Congratulations!~n"
                      "   It took ~p total guesses.~n"
                      "*********************************************~n~n",
                      [Totalguesses]);
        Userguess < Secretnumber ->
            io:format("Your guess is too low.~n~n"),
            %% Move the lower bound up, but never past a tighter existing bound.
            computer_turn(Secretnumber, Totalguesses,
                          max(Lowmax, Userguess + 1), Highmax);
        true ->
            io:format("Your guess is too high.~n~n"),
            %% Move the upper bound down, but never past a tighter existing bound.
            computer_turn(Secretnumber, Totalguesses,
                          Lowmax, min(Highmax, Userguess - 1))
    end.

%% The computer's turn: guess the midpoint of the current bounds (binary
%% search). A correct guess ends the game. Otherwise we narrow the bounds and
%% check the round's taunts before looping.
computer_turn(Secretnumber, Totalguesses, Lowmax, Highmax) ->
    Computerguess = (Lowmax + Highmax) div 2,
    Totalguesses1 = Totalguesses + 1,
    if
        Computerguess =:= Secretnumber ->
            io:format("**********************************************~n"
                      "   The computer's guess of ~p is correct!~n"
                      "   It took ~p total guesses.~n"
                      "**********************************************~n~n",
                      [Computerguess, Totalguesses1]);
        Computerguess < Secretnumber ->
            io:format("The computer guessed ~p and that was too low.~n"
                      "Please try again.~n~n", [Computerguess]),
            after_round(Secretnumber, Totalguesses1,
                        max(Lowmax, Computerguess + 1), Highmax);
        true ->
            io:format("The computer guessed ~p and that was too high.~n"
                      "Please try again.~n~n", [Computerguess]),
            after_round(Secretnumber, Totalguesses1,
                        Lowmax, min(Highmax, Computerguess - 1))
    end.

%% End-of-round taunts and the forced game-over limit. Loop unless the limit
%% ends the game.
after_round(Secretnumber, Totalguesses, Lowmax, Highmax) ->
    case time_taunt(Totalguesses) of
        game_over -> ok;
        continue  -> game_loop(Secretnumber, Totalguesses, Lowmax, Highmax)
    end.

%% Draw the secret number from the GMN_SECRET env hook, or a random 1..100.
secret_from_env() ->
    case os:getenv("GMN_SECRET") of
        false -> rand:uniform(100);
        Value ->
            case string:to_integer(Value) of
                {N, ""} when N >= 1, N =< 100 -> N;
                _ -> rand:uniform(100)
            end
    end.

%% Read one guess from stdin. io:get_line/1 keeps the trailing newline and
%% returns 'eof' when the stream closes; trim the newline and exit cleanly on
%% eof so a closed pipe does not loop forever.
read_guess() ->
    case io:get_line("What is your guess? ") of
        eof  -> halt(0);
        Line -> string:trim(Line)
    end.

%% Classify a raw guess. Matches the Python lineage: a non-numeric entry and an
%% out-of-range number get distinct messages.
parse_guess(Raw) ->
    case is_all_digits(Raw) of
        false ->
            not_a_number;
        true ->
            Userguess = list_to_integer(Raw),
            case Userguess >= 1 andalso Userguess =< 100 of
                true  -> {ok, Userguess};
                false -> out_of_range
            end
    end.

%% True only for a non-empty run of decimal digits (so "", "-5", and "4x" are
%% all rejected as not-a-number, matching Python's str.isdigit check).
is_all_digits("") ->
    false;
is_all_digits(Str) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Str).

%% Taunt for a guess below a bound already known to be too low.
maybe_careless_low(Userguess, Lowmax) when Userguess < Lowmax ->
    io:format("That guess was lower than a previous guess that was too low. "
              "Pay attention!~n~n");
maybe_careless_low(_Userguess, _Lowmax) ->
    ok.

%% Taunt for a guess above a bound already known to be too high.
maybe_careless_high(Userguess, Highmax) when Userguess > Highmax ->
    io:format("Wake up! That guess was higher than an earlier guess that was "
              "too high.~n~n");
maybe_careless_high(_Userguess, _Highmax) ->
    ok.

%% Taunts at 8 and 12 total guesses; a forced game over at 16. Returns whether
%% the game should continue.
time_taunt(Totalguesses) when Totalguesses =:= 8 ->
    io:format("~nThis is a hard number, isn't it?~n~n"),
    continue;
time_taunt(Totalguesses) when Totalguesses =:= 12 ->
    io:format("~nWow! You are really bad at this.~n~n"),
    continue;
time_taunt(Totalguesses) when Totalguesses >= 16 ->
    io:format("~nYou're taking too long, I can't handle it any more.~n~n"
              "G A M E   O V E R~n~n"),
    game_over;
time_taunt(_Totalguesses) ->
    continue.

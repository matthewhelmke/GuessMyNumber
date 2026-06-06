{ Guess My Number - Pascal

  --A Pascal version of a silly game I made on my programmable
    calculator when I was bored in math class in 1987, with a couple of
    additions like input validation and computer guesses.

  Copyright (c) 2007 Matthew Helmke for the old Python 2 version
  Copyright (c) 2026 Matthew Helmke for the Pascal version (this one)

  This Pascal version is a direct behavioral port of the canonical Python
  version. I built it with Claude Code using Claude Opus 4.8, then reviewed
  it myself.

  To compile (Free Pascal):
    fpc guessnumber.pas
  To run:
    ./guessnumber

  Language notes (Pascal choices a reader should not mistake for bugs):

    * Free Pascal's Val converts a string to an integer and reports failure
      through a result code. The guess is first checked to be all digits,
      then Val parses it; a non-zero code (an overflow on a very long run of
      digits) is treated as out of range, matching how Python's int() accepts
      the value and the range check then rejects it.

    * GetEnvironmentVariable (from SysUtils) reads the GMN_SECRET parity hook.
      An empty string means the variable is unset, so the secret stays random.

    * The input loop checks EOF before ReadLn so a closed stdin ends the game
      cleanly instead of looping on empty reads. The prompt is printed first,
      mirroring the other versions, so a piped run that converges never reaches
      the EOF branch.

  This program is free software; you can redistribute it and\or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. }

program guessnumber;

uses SysUtils;

var
  secretnumber, userguess, computerguess: Integer;
  totalguesses, lowmax, highmax, code: Integer;
  userguessunvalidated, gmnSecret: string;

{ True only for a non-empty string of ASCII digits. }
function IsAllDigits(const s: string): Boolean;
var
  i: Integer;
begin
  IsAllDigits := Length(s) > 0;
  for i := 1 to Length(s) do
    if not (s[i] in ['0'..'9']) then
    begin
      IsAllDigits := False;
      Exit;
    end;
end;

begin
  { Print a description of the game, with rules, to the screen }
  WriteLn('Welcome to Guess My Number!');
  WriteLn;
  WriteLn('The computer will select a random whole number between 1 and 100.');
  WriteLn('Your goal is to guess that number. You will get a turn, then a computer');
  WriteLn('player will get a turn. Each of you are aware of the other''s guesses.');
  WriteLn('The first one to guess the number correctly will win. Try to guess in');
  WriteLn('as few turns as possible.');
  WriteLn;
  WriteLn('Here we go!');
  WriteLn;
  WriteLn;

  { Get a random number, or a fixed one from GMN_SECRET for parity testing }
  Randomize;
  secretnumber := Random(100) + 1;
  gmnSecret := GetEnvironmentVariable('GMN_SECRET');
  if IsAllDigits(gmnSecret) then
  begin
    Val(gmnSecret, userguess, code);
    if (code = 0) and (userguess >= 1) and (userguess <= 100) then
      secretnumber := userguess;
  end;

  { set all our initial values }
  userguess := 0;
  totalguesses := 0;
  lowmax := 1;
  highmax := 100;

  { the main bit }
  while True do
  begin
    Write('What is your guess? ');

    { let the user input any number they want }
    if EOF then Break;   { end of input }
    ReadLn(userguessunvalidated);

    { remove leading and trailing spaces }
    userguessunvalidated := Trim(userguessunvalidated);

    { verify the guess is a whole number }
    if not IsAllDigits(userguessunvalidated) then
    begin
      WriteLn('Only whole numbers from 1 to 100 are allowed.');
      WriteLn('Please try again.');
      WriteLn;
      Continue;
    end;

    { make sure the guess is an integer in the right range }
    Val(userguessunvalidated, userguess, code);
    if (code <> 0) or (userguess < 1) or (userguess > 100) then
    begin
      WriteLn('Only whole numbers from 1 to 100 are allowed. Your guess is out of range.');
      WriteLn('Please try again.');
      WriteLn;
      Continue;
    end;

    { this is a real guess, so count it }
    totalguesses := totalguesses + 1;

    { some taunts for silly errors in user guesses }
    if userguess < lowmax then
    begin
      WriteLn('That guess was lower than a previous guess that was too low. Pay attention!');
      WriteLn;
    end;
    if userguess > highmax then
    begin
      WriteLn('Wake up! That guess was higher than an earlier guess that was too high.');
      WriteLn;
    end;

    { evaluate the guess }
    if userguess = secretnumber then
    begin
      WriteLn;
      WriteLn('*********************************************');
      WriteLn('   Your guess is correct! Congratulations!');
      WriteLn('   It took ', totalguesses, ' total guesses.');
      WriteLn('*********************************************');
      WriteLn;
      Break;
    end
    else if userguess > secretnumber then
    begin
      WriteLn('Your guess is too high.');
      WriteLn;
      if userguess <= highmax then
        highmax := userguess - 1;
    end
    else
    begin
      WriteLn('Your guess is too low.');
      WriteLn;
      if userguess >= lowmax then
        lowmax := userguess + 1;
    end;

    { the computer's guess uses a binary-search midpoint within current bounds }
    computerguess := (lowmax + highmax) div 2;
    totalguesses := totalguesses + 1;

    if computerguess = secretnumber then
    begin
      WriteLn('**********************************************');
      WriteLn('   The computer''s guess of ', computerguess, ' is correct!');
      WriteLn('   It took ', totalguesses, ' total guesses.');
      WriteLn('**********************************************');
      WriteLn;
      Break;
    end
    else if computerguess > secretnumber then
    begin
      WriteLn('The computer guessed ', computerguess, ' and that was too high.');
      WriteLn('Please try again.');
      WriteLn;
      highmax := computerguess - 1;
    end
    else
    begin
      WriteLn('The computer guessed ', computerguess, ' and that was too low.');
      WriteLn('Please try again.');
      WriteLn;
      lowmax := computerguess + 1;
    end;

    { more taunts and a forced guess limit }
    if totalguesses = 8 then
    begin
      WriteLn;
      WriteLn('This is a hard number, isn''t it?');
      WriteLn;
    end
    else if totalguesses = 12 then
    begin
      WriteLn;
      WriteLn('Wow! You are really bad at this.');
      WriteLn;
    end
    else if totalguesses >= 16 then
    begin
      WriteLn;
      WriteLn('You''re taking too long, I can''t handle it any more.');
      WriteLn;
      WriteLn('G A M E   O V E R');
      WriteLn;
      Break;
    end;
  end;
end.

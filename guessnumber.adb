--  Guess my number game
--
--  An Ada version of a silly game I made on my programmable
--  calculator when I was bored in math class in 1987, with a couple of
--  additions like input validation and computer guesses.
--
--  Copyright (c) 2026 Matthew Helmke for the Ada version (this one)
--     with assistance from Claude Code (claude.ai/code)
--     using the claude-opus-5-5 model
--
--  This Ada version is a direct behavioral port of the canonical Python
--  version.
--
--  To compile (GNAT):
--    gnatmake guessnumber.adb
--  To run:
--    ./guessnumber
--
--  Language notes (Ada choices a reader should not mistake for bugs):
--
--    * GNAT expects a file to be named after the unit it holds, so the main
--      procedure Guessnumber lives in guessnumber.adb.
--
--    * Ada ignores case, so secretnumber and Secretnumber are the same name.
--      The shared variable names are written in lower case, as in the other
--      versions, while the helper names follow Ada's usual Mixed_Case style.
--      For the same reason, a variable cannot share a type's name: status
--      and Guess_Status are fine, but guess_status would clash.
--
--    * Ada has no continue statement. Parse_Guess sorts each input into
--      Not_A_Number, Out_Of_Range, or Valid, and a case statement handles
--      each one, so only a valid guess reaches the rest of the turn. An exit
--      statement ends the game loop the way break does in Python.
--
--    * Integer'Value would accept far more than a guess should: a sign,
--      surrounding spaces, an underscore between digits, and based literals
--      such as 16#1E#. The guess is therefore checked to be all digits
--      first. A run of digits too long for an Integer raises
--      Constraint_Error, which is reported as out of range, matching how
--      Python's int() accepts the value and the range check then rejects it.
--
--    * Ada.Strings.Fixed.Trim removes only spaces when given a side such as
--      Both. Given a character set instead, it removes spaces and tabs.
--
--    * Integer'Image puts a space before a positive number, where a minus
--      sign would go. The Image function here drops it.
--
--    * Get_Line raises End_Error when the input runs out, rather than
--      returning a special value. A handler at the end of the procedure
--      catches it and ends the game quietly. A last line with no trailing
--      newline is still returned as a guess first. When the program ends,
--      Ada.Text_IO finishes any line left open, so a game cut short this
--      way ends its last prompt with a newline.
--
--    * Discrete_Random's Reset seeds the generator from the clock, so each
--      run draws a fresh secret number.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Environment_Variables;
with Ada.Numerics.Discrete_Random;

procedure Guessnumber is

   --  a random number generator that draws whole numbers from 1 to 100
   subtype Secret_Range is Integer range 1 .. 100;
   package Random_Secret is new Ada.Numerics.Discrete_Random (Secret_Range);

   --  the three things an input line can turn out to be
   type Guess_Status is (Not_A_Number, Out_Of_Range, Valid);

   --  the characters removed from both ends of a guess: spaces and tabs
   Blanks : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (' ' & ASCII.HT);

   --  True only for a non-empty string of ASCII digits
   function All_Digits (Text : String) return Boolean is
   begin
      if Text'Length = 0 then
         return False;
      end if;
      for Index in Text'Range loop
         if Text (Index) not in '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end All_Digits;

   --  Sort the text into a status, and set Value when it is all digits
   procedure Parse_Guess
     (Text   : String;
      Status : out Guess_Status;
      Value  : out Integer) is
   begin
      Value := 0;
      if not All_Digits (Text) then
         Status := Not_A_Number;
      else
         Value := Integer'Value (Text);
         if Value < 1 or Value > 100 then
            Status := Out_Of_Range;
         else
            Status := Valid;
         end if;
      end if;
   exception
      --  too many digits to fit in an Integer, so certainly above 100
      when Constraint_Error =>
         Status := Out_Of_Range;
   end Parse_Guess;

   --  A number as text, without the leading space Integer'Image adds
   function Image (Number : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Integer'Image (Number), Ada.Strings.Left);
   end Image;

   --  A fixed secret from GMN_SECRET for parity testing, otherwise a random one
   function Secret_From_Env return Integer is
      Generator : Random_Secret.Generator;
      Status    : Guess_Status;
      Value     : Integer;
   begin
      if Ada.Environment_Variables.Exists ("GMN_SECRET") then
         Parse_Guess (Ada.Environment_Variables.Value ("GMN_SECRET"),
                      Status, Value);
         if Status = Valid then
            return Value;
         end if;
      end if;
      Random_Secret.Reset (Generator);
      return Random_Secret.Random (Generator);
   end Secret_From_Env;

   secretnumber  : Integer;
   userguess     : Integer;
   totalguesses  : Integer := 0;
   lowmax        : Integer := 1;
   highmax       : Integer := 100;
   computerguess : Integer;
   status        : Guess_Status;

begin

   --  Print a description of the game, with rules, to the screen
   Put_Line ("Welcome to Guess My Number!");
   New_Line;
   Put_Line ("The computer will select a random whole number between 1 and 100.");
   Put_Line ("Your goal is to guess that number. You will get a turn, then a computer");
   Put_Line ("player will get a turn. Each of you are aware of the other's guesses.");
   Put_Line ("The first one to guess the number correctly will win. Try to guess in");
   Put_Line ("as few turns as possible.");
   New_Line;
   Put_Line ("Here we go!");
   New_Line;
   New_Line;

   secretnumber := Secret_From_Env;

   --  the main bit
   loop

      Put ("What is your guess? ");

      declare
         --  let the user input any number they want
         userguessunvalidated : constant String := Get_Line;
      begin
         --  remove leading and trailing spaces and tabs, then check the guess
         Parse_Guess
           (Ada.Strings.Fixed.Trim (userguessunvalidated, Blanks, Blanks),
            status, userguess);
      end;

      case status is

         when Not_A_Number =>
            Put_Line ("Only whole numbers from 1 to 100 are allowed.");
            Put_Line ("Please try again.");
            New_Line;

         when Out_Of_Range =>
            Put_Line ("Only whole numbers from 1 to 100 are allowed. "
                      & "Your guess is out of range.");
            Put_Line ("Please try again.");
            New_Line;

         when Valid =>

            --  this is a real guess, so count it
            totalguesses := totalguesses + 1;

            --  some taunts for silly errors in user guesses
            if userguess < lowmax then
               Put_Line ("That guess was lower than a previous guess "
                         & "that was too low. Pay attention!");
               New_Line;
            end if;
            if userguess > highmax then
               Put_Line ("Wake up! That guess was higher than an earlier "
                         & "guess that was too high.");
               New_Line;
            end if;

            --  evaluate the guess
            if userguess > secretnumber then
               Put_Line ("Your guess is too high.");
               New_Line;
               --  don't let the computer choose this number or anything
               --  higher as long as a reasonable user guess was made
               if userguess <= highmax then
                  highmax := userguess - 1;
               end if;
            end if;

            if userguess < secretnumber then
               Put_Line ("Your guess is too low.");
               New_Line;
               --  don't let the computer choose this number or anything
               --  lower as long as a reasonable user guess was made
               if userguess >= lowmax then
                  lowmax := userguess + 1;
               end if;
            end if;

            if userguess = secretnumber then
               New_Line;
               Put_Line ("*********************************************");
               Put_Line ("   Your guess is correct! Congratulations!");
               Put_Line ("   It took " & Image (totalguesses)
                         & " total guesses.");
               Put_Line ("*********************************************");
               New_Line;
               exit;
            end if;

            --  the computer's guess is the midpoint of the current bounds
            computerguess := (lowmax + highmax) / 2;
            totalguesses := totalguesses + 1;

            if computerguess > secretnumber then
               Put_Line ("The computer guessed " & Image (computerguess)
                         & " and that was too high.");
               Put_Line ("Please try again.");
               New_Line;
               highmax := computerguess - 1;
            end if;

            if computerguess < secretnumber then
               Put_Line ("The computer guessed " & Image (computerguess)
                         & " and that was too low.");
               Put_Line ("Please try again.");
               New_Line;
               lowmax := computerguess + 1;
            end if;

            if computerguess = secretnumber then
               Put_Line ("**********************************************");
               Put_Line ("   The computer's guess of " & Image (computerguess)
                         & " is correct!");
               Put_Line ("   It took " & Image (totalguesses)
                         & " total guesses.");
               Put_Line ("**********************************************");
               New_Line;
               exit;
            end if;

            --  these taunts are just for my amusement and to keep the game
            --  from being too terribly long
            if totalguesses = 8 then
               New_Line;
               Put_Line ("This is a hard number, isn't it?");
               New_Line;
            end if;

            if totalguesses = 10 then
               New_Line;
               Put_Line ("Wow! You are really bad at this.");
               New_Line;
            end if;

            if totalguesses >= 12 then
               New_Line;
               Put_Line ("You're taking too long, I can't handle it any more.");
               New_Line;
               Put_Line ("G A M E   O V E R");
               New_Line;
               exit;
            end if;

      end case;

   end loop;

exception
   --  the input ran out before the game ended, so stop quietly
   when End_Error =>
      null;
end Guessnumber;

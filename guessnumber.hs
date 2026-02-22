#!/usr/bin/env runghc
{-
  Guess My Number - Haskell

  --A Haskell version of a silly game I made on my programmable
    calculator when I was bored in math class in 1987, with a couple of
    additions like input validation and computer guesses.

  Copyright (c) 2026 Matthew Helmke for the Haskell version (this one)

  This Haskell version is a direct behavioral port of the Python, Bash, C,
  Perl, PHP, Racket, COBOL, Go, Ruby, Java, Fortran, and JavaScript versions.
  I used GitHub Copilot, which at this moment used Claude Haiku 4.5 • 1x, in the
  creation of this port, but then edited it further myself.

  To run:
    runghc guessnumber.hs

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
-}

import System.Random (randomRIO)
import Text.Read (readMaybe)
import Control.Monad (when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn "Welcome to Guess My Number!\n"
  putStrLn "The computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n"
  putStrLn "Here we go!\n"

  secretnumber <- randomRIO (1,100) :: IO Int

  -- Start with shared bounds: lowmax=1 (no lower bound yet), highmax=100
  gameLoop secretnumber 1 1 100

gameLoop :: Int -> Int -> Int -> Int -> IO ()
gameLoop secretnumber totalguesses lowmax highmax = do
  putStr "What is your guess? "
  userguessunvalidated <- getLine
  let totalguesses' = totalguesses + 1
  case readMaybe userguessunvalidated :: Maybe Int of
    Nothing -> do
      putStrLn "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n"
      gameLoop secretnumber totalguesses lowmax highmax
    Just userguess -> do
      if userguess < 1 || userguess > 100
        then do
          putStrLn "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n"
          gameLoop secretnumber totalguesses lowmax highmax
        else do
          -- silly taunts for careless guesses (based on shared bounds)
          when (userguess < lowmax) $
            putStrLn "That guess was lower than a previous guess that was too low. Pay attention!"
          when (userguess > highmax) $
            putStrLn "Wake up! That guess was higher than an earlier guess that was too high."

          -- evaluate user's guess
          if userguess == secretnumber
            then do
              putStrLn "\n*********************************************"
              putStrLn $ "   Your guess is correct! Congratulations!"
              putStrLn $ "   It took " ++ show totalguesses' ++ " total guesses."
              putStrLn "*********************************************\n"
              putStrLn "Press Enter to exit."
              _ <- getLine
              exitSuccess
            else do
              (lowmax', highmax') <-
                if userguess < secretnumber
                  then do
                    putStrLn "Your guess is too low.\n"
                    let lb = max lowmax (userguess + 1)
                    return (lb, highmax)
                  else do
                    putStrLn "Your guess is too high.\n"
                    let hb = min highmax (userguess - 1)
                    return (lowmax, hb)

              -- time-based taunts / limits
              when (totalguesses' == 8) $ putStrLn "\nThis is a hard number, isn't it?\n"
              when (totalguesses' == 12) $ putStrLn "\nWow! You are really bad at this.\n"
              when (totalguesses' >= 16) $ do
                putStrLn "\nYou're taking too long, I can't handle it any more.\n\n"
                putStrLn "G A M E   O V E R"
                putStrLn "\nPress Enter to exit."
                _ <- getLine
                exitSuccess

              -- Computer's turn: binary-search midpoint within bounds
              let computerguess = if lowmax' > highmax'
                                    then lowmax'
                                    else (lowmax' + highmax') `div` 2
              let totalguesses'' = totalguesses' + 1

              -- evaluate computer guess
              if computerguess == secretnumber
                then do
                  putStrLn "**********************************************"
                  putStrLn $ "   The computer's guess of " ++ show computerguess ++ " is correct!"
                  putStrLn $ "   It took " ++ show totalguesses'' ++ " total guesses."
                  putStrLn "**********************************************\n"
                  putStrLn "Press Enter to exit."
                  _ <- getLine
                  exitSuccess
                else do
                  (lowmax'', highmax'') <-
                    if computerguess < secretnumber
                      then do
                        putStrLn $ "The computer guessed " ++ show computerguess ++ " and that was too low."
                        putStrLn "Please try again.\n"
                        let lb2 = max lowmax' (computerguess + 1)
                        return (lb2, highmax')
                      else do
                        putStrLn $ "The computer guessed " ++ show computerguess ++ " and that was too high."
                        putStrLn "Please try again.\n"
                        let hb2 = min highmax' (computerguess - 1)
                        return (lowmax', hb2)

                  -- taunts after computer guess
                  when (totalguesses'' == 8) $ putStrLn "\nThis is a hard number, isn't it?\n"
                  when (totalguesses'' == 12) $ putStrLn "\nWow! You are really bad at this.\n"
                  when (totalguesses'' >= 16) $ do
                    putStrLn "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n"
                    putStrLn "Press Enter to exit."
                    _ <- getLine
                    exitSuccess

                  -- loop with updated state
                  gameLoop secretnumber totalguesses'' lowmax'' highmax''

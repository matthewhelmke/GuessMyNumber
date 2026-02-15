#!/usr/bin/env runghc
{-
  Guess My Number - Haskell

  --A Haskell version of a silly game I made on my programmable
    calculator when I was bored in math class in 1987, with a couple of
    additions like input validation and computer guesses.

  Copyright (c) 2026 Matthew Helmke for the Haskell version (this one)

  This Haskell version is a direct behavioral port of the Python, Bash, C,
  Perl, PHP, Racket, COBOL, Go, Ruby, Java, Fortran, and JavaScript versions.
  I used GitHub Copilot, which at this moment used Claude Haiku 4.5, in the
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
  putStrLn "The computer will select a random whole number between 1 and 100."
  putStrLn "Your goal is to guess that number. You will get a turn, then a computer player will get a turn. Each of you are aware of the other's guesses. The first one to guess the number correctly will win. Try to guess in as few turns as possible.\n"
  putStrLn "Here we go!\n"

  secret <- randomRIO (1,100) :: IO Int

  -- State: total guesses, lowBound, highBound, lowestTooLow, highestTooHigh
  -- lowestTooLow starts at 0 so no initial taunt; highestTooHigh starts at 101
  gameLoop secret 0 1 100 0 101

gameLoop :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
gameLoop secret total lowBound highBound lowestTooLow highestTooHigh = do
  putStr "What is your guess? "
  line <- getLine
  case readMaybe line :: Maybe Int of
    Nothing -> do
      putStrLn "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n"
      gameLoop secret total lowBound highBound lowestTooLow highestTooHigh
    Just userGuess -> do
      if userGuess < 1 || userGuess > 100
        then do
          putStrLn "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n"
          gameLoop secret total lowBound highBound lowestTooLow highestTooHigh
        else do
          -- silly taunts for careless guesses
          when (userGuess <= lowestTooLow) $
            putStrLn "That guess was lower than a previous guess that was too low. Pay attention!"
          when (userGuess >= highestTooHigh) $
            putStrLn "Wake up! That guess was higher than an earlier guess that was too high."

          let total' = total + 1

          -- evaluate user's guess
          if userGuess == secret
            then do
              putStrLn "\n*********************************************"
              putStrLn $ "   Your guess is correct! Congratulations!"
              putStrLn $ "   It took " ++ show total' ++ " total guesses."
              putStrLn "*********************************************\n"
              putStrLn "Press Enter to exit."
              _ <- getLine
              exitSuccess
            else do
              (lowBound', highBound', lowestTooLow', highestTooHigh') <-
                if userGuess < secret
                  then do
                    putStrLn "Your guess is too low.\n"
                    let lt = max lowestTooLow userGuess
                    let lb = max lowBound (userGuess + 1)
                    return (lb, highBound, lt, highestTooHigh)
                  else do
                    putStrLn "Your guess is too high.\n"
                    let ht = min highestTooHigh userGuess
                    let hb = min highBound (userGuess - 1)
                    return (lowBound, hb, lowestTooLow, ht)

              -- time-based taunts / limits
              when (total' == 8) $ putStrLn "\nThis is a hard number, isn't it?\n"
              when (total' == 12) $ putStrLn "\nWow! You are really bad at this.\n"
              when (total' >= 16) $ do
                putStrLn "\nYou're taking too long, I can't handle it any more.\n\n"
                putStrLn "G A M E   O V E R"
                putStrLn "\nPress Enter to exit."
                _ <- getLine
                exitSuccess

              -- Computer's turn: binary-search midpoint within bounds
              let compGuess = if lowBound' > highBound'
                                then lowBound'
                                else (lowBound' + highBound') `div` 2
              let total'' = total' + 1

              -- evaluate computer guess
              if compGuess == secret
                then do
                  putStrLn "**********************************************"
                  putStrLn $ "   The computer's guess of " ++ show compGuess ++ " is correct!"
                  putStrLn $ "   It took " ++ show total'' ++ " total guesses."
                  putStrLn "**********************************************\n"
                  putStrLn "Press Enter to exit."
                  _ <- getLine
                  exitSuccess
                else do
                  (lowBound'', highBound'', lowestTooLow'', highestTooHigh'') <-
                    if compGuess < secret
                      then do
                        putStrLn $ "The computer guessed " ++ show compGuess ++ " and that was too low."
                        putStrLn "Please try again.\n"
                        let lb2 = max lowBound' (compGuess + 1)
                        let lt2 = max lowestTooLow' compGuess
                        return (lb2, highBound', lt2, highestTooHigh')
                      else do
                        putStrLn $ "The computer guessed " ++ show compGuess ++ " and that was too high."
                        putStrLn "Please try again.\n"
                        let hb2 = min highBound' (compGuess - 1)
                        let ht2 = min highestTooHigh' compGuess
                        return (lowBound', hb2, lowestTooLow', ht2)

                  -- taunts after computer guess
                  when (total'' == 8) $ putStrLn "\nThis is a hard number, isn't it?\n"
                  when (total'' == 12) $ putStrLn "\nWow! You are really bad at this.\n"
                  when (total'' >= 16) $ do
                    putStrLn "\nYou're taking too long, I can't handle it any more.\n\n"
                    putStrLn "G A M E   O V E R\n"
                    putStrLn "Press Enter to exit."
                    _ <- getLine
                    exitSuccess

                  -- loop with updated state
                  gameLoop secret total'' lowBound'' highBound'' lowestTooLow'' highestTooHigh''

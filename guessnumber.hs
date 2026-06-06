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
import System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "Welcome to Guess My Number!\n"
  putStrLn "The computer will select a random whole number between 1 and 100.\nYour goal is to guess that number. You will get a turn, then a computer\nplayer will get a turn. Each of you are aware of the other's guesses.\nThe first one to guess the number correctly will win. Try to guess in\nas few turns as possible.\n"
  putStrLn "Here we go!\n"

  -- A fixed secret from GMN_SECRET for parity testing, otherwise a random one
  secretnumber <- secretFromEnv

  -- Start with no guesses yet and shared bounds: lowmax=1, highmax=100
  gameLoop secretnumber 0 1 100

secretFromEnv :: IO Int
secretFromEnv = do
  envValue <- lookupEnv "GMN_SECRET"
  case envValue >>= readMaybe of
    Just n | n >= 1 && n <= 100 -> return n
    _ -> randomRIO (1, 100)

gameLoop :: Int -> Int -> Int -> Int -> IO ()
gameLoop secretnumber totalguesses lowmax highmax = do
  putStr "What is your guess? "
  userguessunvalidated <- getLine
  case readMaybe userguessunvalidated :: Maybe Int of
    Nothing -> do
      putStrLn "Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n"
      gameLoop secretnumber totalguesses lowmax highmax
    Just userguess
      | userguess < 1 || userguess > 100 -> do
          putStrLn "Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n"
          gameLoop secretnumber totalguesses lowmax highmax
      | otherwise -> do
          -- this is a real guess, so count it
          let totalguesses' = totalguesses + 1

          -- silly taunts for careless guesses (based on shared bounds)
          when (userguess < lowmax) $
            putStrLn "That guess was lower than a previous guess that was too low. Pay attention!\n"
          when (userguess > highmax) $
            putStrLn "Wake up! That guess was higher than an earlier guess that was too high.\n"

          -- evaluate the user's guess
          if userguess == secretnumber
            then do
              putStrLn "\n*********************************************"
              putStrLn "   Your guess is correct! Congratulations!"
              putStrLn $ "   It took " ++ show totalguesses' ++ " total guesses."
              putStrLn "*********************************************\n"
              exitSuccess
            else do
              (lowmax', highmax') <-
                if userguess > secretnumber
                  then do
                    putStrLn "Your guess is too high.\n"
                    return (lowmax, min highmax (userguess - 1))
                  else do
                    putStrLn "Your guess is too low.\n"
                    return (max lowmax (userguess + 1), highmax)

              -- the computer's guess uses the midpoint of the current bounds
              let computerguess = (lowmax' + highmax') `div` 2
              let totalguesses'' = totalguesses' + 1

              if computerguess == secretnumber
                then do
                  putStrLn "**********************************************"
                  putStrLn $ "   The computer's guess of " ++ show computerguess ++ " is correct!"
                  putStrLn $ "   It took " ++ show totalguesses'' ++ " total guesses."
                  putStrLn "**********************************************\n"
                  exitSuccess
                else do
                  (lowmax'', highmax'') <-
                    if computerguess > secretnumber
                      then do
                        putStrLn $ "The computer guessed " ++ show computerguess ++ " and that was too high."
                        putStrLn "Please try again.\n"
                        return (lowmax', computerguess - 1)
                      else do
                        putStrLn $ "The computer guessed " ++ show computerguess ++ " and that was too low."
                        putStrLn "Please try again.\n"
                        return (computerguess + 1, highmax')

                  -- more taunts and a forced guess limit
                  when (totalguesses'' == 8) $ putStrLn "\nThis is a hard number, isn't it?\n"
                  when (totalguesses'' == 12) $ putStrLn "\nWow! You are really bad at this.\n"
                  when (totalguesses'' >= 16) $ do
                    putStrLn "\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n"
                    exitSuccess

                  -- loop with updated state
                  gameLoop secretnumber totalguesses'' lowmax'' highmax''

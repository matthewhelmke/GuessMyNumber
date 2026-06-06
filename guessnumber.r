#!/usr/bin/env Rscript

#
# Guess my number game
#
# --An R version of a silly game I made on my programmable
#   calculator when I was bored in math class in 1987, with a couple of
#   additions like input validation and computer guesses.
#
# Original versions:
#   Copyright (c) 2007 Matthew Helmke
#   Copyright (c) 2025 Matthew Helmke
#
# This R version is a direct behavioral port of the Rust version,
# which itself was derived from the Python, Bash, C, Perl, PHP,
# Racket, COBOL, Go, Ruby, Java, Fortran, and JavaScript versions.
# I used ChatGPT in the creation of this port, but then edited it further
# myself.
#
# Note: this implementation avoids readline() and uses readLines("stdin")
# to ensure correct blocking behavior under Rscript.
#
# To run on Linux, make sure R is installed and then:
#
# $ Rscript guessmynumber.r
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License.
#

# Open stdin once and reuse the connection. readLines("stdin", ...) opens a
# fresh connection each call, which under Rscript reads only the first line
# and then sees EOF on every subsequent call.
.stdin_conn <- file("stdin", open = "r")

read_input <- function(prompt = "") {
  cat(prompt)
  flush.console()

  line <- tryCatch(
    readLines(.stdin_conn, n = 1),
    error = function(e) character(0)
  )

  if (length(line) == 0) {
    return(NA_character_)
  }

  trimws(line)
}

cat("Welcome to Guess My Number!\n\n")
cat("The computer will select a random whole number between 1 and 100.\n")
cat("Your goal is to guess that number. You will get a turn, then a computer\n")
cat("player will get a turn. Each of you are aware of the other's guesses.\n")
cat("The first one to guess the number correctly will win. Try to guess in\n")
cat("as few turns as possible.\n\n")
cat("Here we go!\n\n")

# A fixed secret from GMN_SECRET for parity testing, otherwise a random one
gmn_secret <- Sys.getenv("GMN_SECRET")
if (grepl("^[0-9]+$", gmn_secret) && as.integer(gmn_secret) >= 1 && as.integer(gmn_secret) <= 100) {
  secretnumber <- as.integer(gmn_secret)
} else {
  secretnumber <- sample(1:100, 1)
}

totalguesses <- 0
lowmax <- 1
highmax <- 100

repeat {
  guess_input <- read_input("What is your guess? ")
  if (is.na(guess_input)) {
    break
  }

  # verify the guess is a whole number
  if (!grepl("^[0-9]+$", guess_input)) {
    cat("Only whole numbers from 1 to 100 are allowed.\nPlease try again.\n\n")
    next
  }

  # verify the guess is in range
  userguess <- as.integer(guess_input)
  if (userguess < 1 || userguess > 100) {
    cat("Only whole numbers from 1 to 100 are allowed. Your guess is out of range.\nPlease try again.\n\n")
    next
  }

  # this is a real guess, so count it
  totalguesses <- totalguesses + 1

  # some taunts for silly errors in user guesses
  if (userguess < lowmax) {
    cat("That guess was lower than a previous guess that was too low. Pay attention!\n\n")
  }
  if (userguess > highmax) {
    cat("Wake up! That guess was higher than an earlier guess that was too high.\n\n")
  }

  # evaluate the guess
  if (userguess == secretnumber) {
    cat("\n*********************************************\n")
    cat("   Your guess is correct! Congratulations!\n")
    cat(sprintf("   It took %d total guesses.\n", totalguesses))
    cat("*********************************************\n\n")
    break
  } else if (userguess > secretnumber) {
    cat("Your guess is too high.\n\n")
    if (userguess <= highmax) {
      highmax <- userguess - 1
    }
  } else {
    cat("Your guess is too low.\n\n")
    if (userguess >= lowmax) {
      lowmax <- userguess + 1
    }
  }

  # computer uses the midpoint (binary search) within current bounds
  computerguess <- as.integer((lowmax + highmax) / 2)
  totalguesses <- totalguesses + 1

  if (computerguess == secretnumber) {
    cat("**********************************************\n")
    cat(sprintf("   The computer's guess of %d is correct!\n", computerguess))
    cat(sprintf("   It took %d total guesses.\n", totalguesses))
    cat("**********************************************\n\n")
    break
  } else if (computerguess > secretnumber) {
    cat(sprintf("The computer guessed %d and that was too high.\nPlease try again.\n\n", computerguess))
    highmax <- computerguess - 1
  } else {
    cat(sprintf("The computer guessed %d and that was too low.\nPlease try again.\n\n", computerguess))
    lowmax <- computerguess + 1
  }

  # more taunts and a forced guess limit
  if (totalguesses == 8) {
    cat("\nThis is a hard number, isn't it?\n\n")
  } else if (totalguesses == 12) {
    cat("\nWow! You are really bad at this.\n\n")
  } else if (totalguesses >= 16) {
    cat("\nYou're taking too long, I can't handle it any more.\n\nG A M E   O V E R\n")
    break
  }
}

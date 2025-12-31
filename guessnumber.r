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

read_input <- function(prompt = "") {
  cat(prompt)
  flush.console()

  line <- tryCatch(
    readLines("stdin", n = 1),
    error = function(e) character(0)
  )

  if (length(line) == 0) {
    return(NA_character_)
  }

  trimws(line)
}

cat("\nWelcome to Guess My Number!\n\n")
cat(
  "The computer will select a random whole number between 1 and 100. ",
  "Your goal is to guess that number. You will get a turn, then a computer ",
  "player will get a turn. Each of you are aware of the other's guesses. ",
  "The first one to guess the number correctly will win. Try to guess in as ",
  "few turns as possible.\n\n",
  sep = ""
)
cat("Here we go!\n\n")

secret <- sample(1:100, 1)

total_guesses <- 0
lowmax <- 1
highmax <- 100

repeat {

  ## ----- User guess -----
  guess_input <- read_input("Your guess: ")

  if (is.na(guess_input)) {
    cat("\nNo input detected. Exiting.\n")
    quit(save = "no")
  }


  if (!nzchar(guess_input)) {
    cat("\nNo input detected. Exiting.\n")
    quit(save = "no")
  }

  user_guess <- suppressWarnings(as.integer(guess_input))

  if (is.na(user_guess)) {
    cat(
      "Only whole numbers from 1 to 100 are allowed. ",
      "Your guess is not a whole number.\nPlease try again.\n\n",
      sep = ""
    )
    next
  }

  if (user_guess < 1 || user_guess > 100) {
    cat(
      "Only whole numbers from 1 to 100 are allowed. ",
      "Your guess is out of range.\nPlease try again.\n\n",
      sep = ""
    )
    next
  }

  total_guesses <- total_guesses + 1

  ## taunts for guesses outside of established limits
  if (user_guess < lowmax) {
    cat("That guess was lower than a previous guess that was too low. Pay attention!\n")
  }

  if (user_guess > highmax) {
    cat("Wake up! That guess was higher than an earlier guess that was too high.\n")
  }

  if (user_guess == secret) {
    cat("\n*********************************************\n")
    cat(sprintf("   Your guess of %d is correct!\n", user_guess))
    cat(sprintf("    It took %d guesses!\n", total_guesses))
    cat("*********************************************\n\n")
    break
  } else if (user_guess < secret) {
    cat("Too low.\n\n")
    if (user_guess >= lowmax) {
      lowmax <- user_guess + 1
    }
  } else {
    cat("Too high.\n\n")
    if (user_guess <= highmax) {
      highmax <- user_guess - 1
    }
  }

  ## PHP-aligned taunt thresholds
  if (total_guesses == 8) {
    cat("\nThis is a hard number, isn't it?\n\n")
  }

  if (total_guesses == 12) {
    cat("\nWow! You are really bad at this.\n\n")
  }

  if (total_guesses >= 16) {
    cat("\nYou're taking too long, I can't handle it any more.\n")
    cat("G A M E   O V E R\n")
    quit(save = "no")
  }

  ## ----- Computer guess -----
  computer_guess <- as.integer((lowmax + highmax) / 2)
  total_guesses <- total_guesses + 1

  cat(sprintf("The computer guesses %d.\n", computer_guess))

  if (computer_guess == secret) {
    cat("\n*********************************************\n")
    cat(sprintf("   The computer's guess of %d is correct!\n", computer_guess))
    cat(sprintf("    It took %d guesses!\n", total_guesses))
    cat("*********************************************\n\n")
    break
  } else if (computer_guess < secret) {
    cat("Too low.\n\n")
    if (computer_guess >= lowmax) {
      lowmax <- computer_guess + 1
    }
  } else {
    cat("Too high.\n\n")
    if (computer_guess <= highmax) {
      highmax <- computer_guess - 1
    }
  }

  if (total_guesses == 8) {
    cat("\nThis is a hard number, isn't it?\n\n")
  }

  if (total_guesses == 12) {
    cat("\nWow! You are really bad at this.\n\n")
  }

  if (total_guesses >= 16) {
    cat("\nYou're taking too long, I can't handle it any more.\n")
    cat("G A M E   O V E R\n")
    quit(save = "no")
  }
}

read_input("Press Enter to exit.")

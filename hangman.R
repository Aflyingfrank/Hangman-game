library(ggplot2)

# Function to update the guessed word based on the correct guess
updateGuessedWord <- function(word, letter, guessedWord) {
  if (regexpr(letter, word)[1] > 0) {
    word <- tolower(word)
    letter <- tolower(letter)
    positions <- which(strsplit(word, "")[[1]] == letter)
    guessedWord[positions] <- letter
    message(paste(guessedWord, collapse = " "))
    
    if (paste(guessedWord, collapse = "") == word) {
      message("Congratulations! You guessed the word!")
      return(guessedWord)
    }
    return(guessedWord)
  }
}

# Function to draw the head of the hangman
drawHead <- function(origin = c(0, 0), diameter = 1, numPoints = 10, group = 5) {
  angleSeq <- seq(0, 2 * pi, length.out = numPoints)
  radius <- diameter / 2
  xData <- origin[1] + radius * cos(angleSeq)
  yData <- origin[2] + radius * sin(angleSeq)
  return(data.frame(x = xData, y = yData, group = group))
}

# Function to draw the hangman based on the number of wrong attempts
drawHangman <- function(wrongAttempts) {
  ggplot(levels[levels$group <= wrongAttempts, ], aes(x = x, y = y, group = group)) +
    geom_path(linewidth = 2.5) + # Fixed the warning by using linewidth
    theme_void() +
    ggtitle('Hangman Game')
}

# Function to check if the letter has been guessed already
checkDuplicateGuess <- function(letter, guesses) {
  if (length(guesses) == 0) {
    message("Starting the game! Checking your guess...")
    guesses <- rbind(guesses, guess = letter)
    guesses[, 1] <- as.character(guesses[, 1])
  } else {
    if (grepl(letter, guesses) == TRUE) {
      guesses[, 1] <- as.character(guesses[, 1])
      message("Already guessed... Try another letter!")
    } else {
      guesses <- rbind(guesses, guess = letter)
      guesses[, 1] <- as.character(guesses[, 1])
      message("Checking your guess...")
    }
  }
  return(guesses)
}

# Data for graph
level1 <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8),
                     y = c(1, 1, 1, 1, 1, 1, 1, 1),
                     group = c(1, 1, 1, 1, 1, 1, 1, 1))
level2 <- data.frame(x = c(4, 4, 4, 4, 4),
                     y = c(1, 2, 3, 4, 5),
                     group = c(2, 2, 2, 2, 2))
level3 <- data.frame(x = c(4, 5, 6), y = c(5, 5, 5), group = c(3, 3, 3))
level4 <- data.frame(x = c(6, 6), y = c(5, 4), group = c(4, 4))
level5 <- drawHead(c(6, 3.5), 1, 100, 5)
level6 <- data.frame(x = c(6, 6, 5.8, 6.2),
                     y = c(3, 1.5, 1.5, 1.5), group = c(6, 6, 6, 6))
level7 <- data.frame(x = c(5.5, 6, 6.5), y = c(2, 2.5, 2), group = c(7, 7, 7))
levels <- rbind(level1, level2, level3, level4, level5, level6, level7)
rm(level1, level2, level3, level4, level5, level6, level7)

# Helper variables
suppressWarnings(rm(wrongAttempts, guesses, letter, targetWord, targetLetters, guessedWord, word, attempts, isActive))
wrongAttempts <- 0
attempts <- 0
guesses <- data.frame(guess = c(NULL))
targetWord <- NULL
targetLetters <- data.frame(guess = c(NULL))
isActive <- TRUE

# Function to start a new game
startNewGame <- function(caseSensitive = TRUE) {
  word <- readline(prompt = "Enter the word: ")
  cat("\f")
  graphics.off()
  if (caseSensitive == FALSE) {
    word <- tolower(word)
  }
  
  guessedWord <- replicate(nchar(word), '_')
  
  while (isActive == TRUE) {
    if (attempts == 0) {
      writeLines(paste(guessedWord, collapse = " "))
    }
    
    letter <- readline(prompt = "Enter a letter: ")
    
    if (nchar(letter) > 1) message("Taking the first letter")
    letter <- substr(letter, 1, 1)
    
    guesses <- checkDuplicateGuess(letter, guesses)
    
    if (grepl(letter, word) == TRUE) {
      targetLetters <- rbind(targetLetters, letter)
      guessedWord <- updateGuessedWord(word, letter, guessedWord)
      message(paste("Good guess!", "Number of attempts:", attempts + 1))
      
      if (paste(tolower(guessedWord), collapse = "") == tolower(word)) {
        isActive <- FALSE
        message("Congratulations, victory!")
        break
      }
      
    } else {
      targetLetters <- checkDuplicateGuess(letter, targetLetters)
      message(paste("Nope!", "Number of attempts:", attempts + 1, "Wrong letters: {", toString(paste0(targetLetters[, 1])), "}"))
      wrongAttempts <- as.integer(nrow(targetLetters))
      print(drawHangman(wrongAttempts))
      
      if (wrongAttempts == 7) {
        isActive <- FALSE
        message("Game Over... You lost!")
        break
      }
    }
    attempts <- attempts + 1
  }
}

# Start a new game
startNewGame()

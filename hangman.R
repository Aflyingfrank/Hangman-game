library(ggplot2)

zamenjaj2 <- function(beseda, crka, iskana_beseda){
  
  if (regexpr(crka, beseda)[1] > 0) {
    
    beseda <- base::tolower(beseda)
    crka <- base::tolower(crka)
    
    pozicije <- which(strsplit(beseda, "")[[1]]==crka)
    iskana_beseda[pozicije] <- crka
    
    message(paste(iskana_beseda, collapse =  " "))
    
    if (paste(iskana_beseda, collapse = "") == beseda) {
      return(iskana_beseda)
      message("Game Over...")
}
    
    return(iskana_beseda)
}
}

drawHead <- function(orig_position = c(0,0),
                     dia = 1,
                     nof_points = 10,
                     group = 5){
  
  vectT <- seq(0,2*pi, length.out = nof_points)
  r <- dia/2
  x_data <- orig_position[1] + r * cos(vectT)
  y_data <- orig_position[2] + r * sin(vectT)
  return(data.frame (x = x_data, y = y_data, group = group))
}

drawMan <- function(st_napak) { #, iskana_beseda) {
  ggplot(levels[which(levels$group <= st_napak), ], aes(x = x, y = y, group = group)) + 
    geom_path(size = 2.5) + 
    theme_void() +
    ggtitle('Hangman Game')
}


CheckDuplicate <- function(crka, izbor){
  if (length(izbor) == 0) {
    message("Start up the game! Checking your guess...")
    izbor <- rbind(izbor, izb=crka)  
    izbor[,1] <- as.character(izbor[,1])
} else {
    if (grepl(crka,izbor) == TRUE) {
    izbor[,1] <- as.character(izbor[,1])
    message("Already exists... Another guess!")
} else {
    izbor <- rbind(izbor, izb=crka)
    izbor[,1] <- as.character(izbor[,1])
    message("Checking your guess...")
}
}
  return(izbor)
}

#Data for graph
level1 <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8), 
                     y = c(1, 1, 1, 1, 1, 1, 1, 1), 
                     group = c(1, 1, 1, 1, 1, 1, 1, 1))
level2 <- data.frame(x = c(4, 4, 4, 4, 4), 
                     y = c(1, 2, 3, 4, 5),
                     group = c(2, 2, 2, 2, 2))
level3 <- data.frame(x = c(4, 5, 6), y= c (5, 5, 5), group = c(3, 3, 3))
level4 <- data.frame(x = c(6, 6), y = c(5, 4), group = c(4, 4))
level5 <- drawHead(c(6, 3.5), 1, 10, 5)
level6 <- data.frame(x = c(6, 6, 5.8, 6.2), 
                     y =c(3, 1.5, 1.5, 1.5), group = c(6, 6, 6, 6))
level7 <- data.frame(x = c(5.5, 6, 6.5), y = c(2, 2.5, 2), group = c(7, 7, 7))
levels <- rbind(level1, level2, level3, level4, level5, level6, level7)
rm(level1, level2, level3, level4, level5, level6, level7)

#Helper variables
suppressWarnings(rm(st_napak, izbor, crka, cilj, cilj_n, iskana_beseda, beseda, i, active))
st_napak = 0
i = 0
izbor = data.frame(izb=c(NULL))
cilj = NULL
cilj_n = data.frame(izb=c(NULL))
active = TRUE

#Hangman
StartNewGame <- function(sensitive.flag = TRUE) { # sensitive.flag: TRUE -> capital letters are available. 
  beseda <- readline(prompt = "Word: ")
  
  cat("\f") 
  graphics.off() 
  if (sensitive.flag == FALSE) {
    beseda <- base::tolower(beseda)
}
  
  iskana_beseda <- replicate(nchar(beseda), '_')
  
  while (active == TRUE) {
    
    if (i == 0) {
    writeLines(paste(iskana_beseda, collapse = " "))
}
    
    crka <- readline(prompt="Enter Letter: ")
    
    if (nchar(crka)>1) message("Taking first letter")
    crka <- substr(crka, 1, 1)
    
    izbor <- CheckDuplicate(crka, izbor)
    
    #iskana_beseda
    if (grepl(crka, beseda) == TRUE) {
      
    cilj <- rbind(cilj, crka)
    iskana_beseda <- zamenjaj2(beseda, crka, iskana_beseda)
       
    message(paste("Good guess!","Number of attempts:",i+1)) 
      
    if (as.character(paste(base::tolower(iskana_beseda), collapse = "")) == base::tolower(beseda)) {
    active == FALSE
    message("Bravo, victory!")
    break
}
      
}   else {
    cilj_n <- CheckDuplicate(crka=crka, izbor=cilj_n)
    message(paste("Nope!", "Number of attempts:", i + 1, "     ", "Wrong letters: {", (toString(paste0(cilj_n[,1]))), "}")) 
    st_napak <- as.integer(nrow(cilj_n))
    print(drawMan(st_napak = st_napak))#,iskana_beseda=paste(iskana_beseda, collapse =  " ") ))
    
    if(as.integer(st_napak) == 7){
      active == FALSE
      break
      message("Game Over...")
    }
    
}
    
    i= i + 1
    
    if(st_napak == 7){
      active == FALSE
      break
      message("Game Over...")
    }
  }
}

#Start new Game
StartNewGame()

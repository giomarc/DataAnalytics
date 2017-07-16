require(ggplot2)

sim.Monty <- function(n = 100, build.hist = FALSE){
  
  doors = c(1,2,3)
  # car randomly placed in one of three doors based on equal weights and probs.
  doorCar <- sample(doors, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
  # contestant picks one of three doors based on equal weights and probs.
  doorChoosen <- sample(doors, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
  simDoors <- data.frame(doorCar = doorCar, doorChoosen = doorChoosen)
  
  # host removes the door that doesn't have a car nor the one choosen by contestant.
  doorRemoved <- apply(as.matrix(simDoors), 1, doorRemoval, doorList = doors)
  simDoors <- cbind(simDoors, doorRemoved = doorRemoved)
  
  # scenario #1 - contestant always switches doors
  doorSwitched <- apply(simDoors[,2:3], 1, doorSwitching)
  simDoors <- cbind(simDoors, doorSwitched = doorSwitched)
  
  # scenario # 2 - contestant always stays on original choice
  doorNotSwitched <- apply(simDoors, 1, function(x) x[2])
  simDoors <- cbind(simDoors, doorNotSwitched = doorNotSwitched)
  
  # assess wins and losses for both scenarios 
  switchWin <- apply(simDoors, 1, function(x) ifelse(x[1]==x[4], 1, 0))
  noSwitchWin <- apply(simDoors, 1, function(x) ifelse(x[1]==x[5], 1,0))
  
  simDoors <- cbind(simDoors, switchWin = switchWin, noSwitchWin = noSwitchWin)
  
  if (build.hist == TRUE){
    print(ggplot(simDoors, aes(x=switchWin)) + geom_bar() + ggtitle("Switching Frequencies"))
    print(ggplot(simDoors, aes(x=noSwitchWin)) + geom_bar() + ggtitle("No Switching Frequencies"))
    #barplot(simDoors$switchWin)
    #barplot(simDoors$noSwitchWin)
  }
  
  # build scenarios tables with frequencies, mean or probability of winning, 
  # variance, wins, losses, and iterations
  a <- c(mean(simDoors$switchWin), 
         var(simDoors$switchWin),
         sd(simDoors$switchWin),
         length(simDoors$switchWin[simDoors$switchWin == 0]), 
         length(simDoors$switchWin[simDoors$switchWin == 1]), n)
  
  b <- c(mean(simDoors$noSwitchWin), 
         var(simDoors$noSwitchWin), 
         sd(simDoors$noSwitchWin),
         length(simDoors$noSwitchWin[simDoors$noSwitchWin == 0]), 
         length(simDoors$noSwitchWin[simDoors$noSwitchWin == 1]), n)
  
  c <- as.data.frame(rbind(a, b))
  row.names(c) <- c("Switching", "No Switching")
  colnames(c) <- c("mean_winProb", "variance", "Std. Dev.", "Losses", "Wins", "Iterations")
  
  return(c)
  
}

sim.Monty(n = 1000, build.hist = TRUE)




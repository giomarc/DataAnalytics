sim.Monty <- function(n = 100, build.hist = FALSE){
  
  doors = c(1,2,3)
  doorCar <- sample(doors, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
  doorChoosen <- sample(doors, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
  simDoors <- data.frame(doorCar = doorCar, doorChoosen = doorChoosen)
  
  
  doorRemoved <- apply(as.matrix(simDoors), 1, doorRemoval, doorList = doors)
  simDoors <- cbind(simDoors, doorRemoved = doorRemoved)
  
  doorSwitched <- apply(simDoors[,2:3], 1, doorSwitching)
  simDoors <- cbind(simDoors, doorSwitched = doorSwitched)
  
  doorNotSwitched <- apply(simDoors, 1, function(x) x[2])
  simDoors <- cbind(simDoors, doorNotSwitched = doorNotSwitched)
  
  switchWin <- apply(simDoors, 1, function(x) ifelse(x[1]==x[4], 1, 0))
  noSwitchWin <- apply(simDoors, 1, function(x) ifelse(x[1]==x[5], 1,0))
  
  simDoors <- cbind(simDoors, switchWin = switchWin, noSwitchWin = noSwitchWin)
  
  sum(simDoors$switchWin)
  
  sum(simDoors$noSwitchWin)
  
  if (build.hist == TRUE){
    hist(simDoors$switchWin)
    hist(simDoors$noSwitchWin)
  }
  
  a <- c(mean(simDoors$switchWin), 
         var(simDoors$switchWin), 
         length(simDoors$switchWin[simDoors$switchWin == 0]), 
         length(simDoors$switchWin[simDoors$switchWin == 1]), n)
  
  b <- c(mean(simDoors$noSwitchWin), 
         var(simDoors$noSwitchWin), 
         length(simDoors$noSwitchWin[simDoors$noSwitchWin == 0]), 
         length(simDoors$noSwitchWin[simDoors$noSwitchWin == 1]), n)
  
  c <- as.data.frame(rbind(a, b))
  row.names(c) <- c("Switching", "No Switching")
  colnames(c) <- c("mean_winProb", "variance", "Wins", "Losses", "Iterations")
  
  return(c)
  
}

sim.Monty(n = 1000, build.hist = TRUE)

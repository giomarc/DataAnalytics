

doors = c(1,2,3)
doorCar <- sample(doors, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
doorChoosen <- sample(doors, size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
simMonty <- data.frame(doorCar = doorCar, doorChoosen = doorChoosen)


doorRemoved <- apply(as.matrix(simMonty), 1, doorRemoval, doorList = doors)
simMonty <- cbind(simMonty, doorRemoved = doorRemoved)

doorSwitched <- apply(simMonty[,2:3], 1, doorSwitching)
simMonty <- cbind(simMonty, doorSwitched = doorSwitched)

doorNotSwitched <- apply(simMonty, 1, function(x) x[2])
simMonty <- cbind(simMonty, doorNotSwitched = doorNotSwitched)

switchWin <- apply(simMonty, 1, function(x) ifelse(x[1]==x[4], 1, 0))
noSwitchWin <- apply(simMonty, 1, function(x) ifelse(x[1]==x[5], 1,0))

simMonty <- cbind(simMonty, switchWin = switchWin, noSwitchWin = noSwitchWin)

sum(simMonty$switchWin)

sum(simMonty$noSwitchWin)


hist(simMonty$switchWin)
hist(simMonty$noSwitchWin)

a <- c(mean(simMonty$switchWin), 
       var(simMonty$switchWin), 
       length(simMonty$switchWin[simMonty$switchWin == 0]), 
       length(simMonty$switchWin[simMonty$switchWin == 1]), n)

b <- c(mean(simMonty$noSwitchWin), 
       var(simMonty$noSwitchWin), 
       length(simMonty$noSwitchWin[simMonty$noSwitchWin == 0]), 
       length(simMonty$noSwitchWin[simMonty$noSwitchWin == 1]), n)

c <- as.data.frame(rbind(a, b))
row.names(c) <- c("Switching", "No Switching")
colnames(c) <- c("mean_winProb", "variance", "Wins", "Losses", "Iterations")

return(c)

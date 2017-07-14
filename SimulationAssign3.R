doors = c(1,2,3)
doorCar <- sample(doors, size = 1000, replace = TRUE, prob = c(1/3, 1/3, 1/3))
doorChoosen <- sample(doors, size = 1000, replace = TRUE, prob = c(1/3, 1/3, 1/3))
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
mean(simMonty$switchWin)
var(simMonty$switchWin)

hist(simMonty$noSwitchWin)
mean(simMonty$noSwitchWin)
var(simMonty$noSwitchWin)
doorRemoval <- function(x, doorList = c(1,2,3)){ # x = row of door chosen and door with car.
  doorList <- as.data.frame(doorList)
  colnames(doorList) <- c("doors")
  # examine door chosen and door with car, get list of doors available for removal (no car, not chosen)
  doorList <- doorList[!(doorList$doors %in% as.list(x)),] 
  ifelse(length(doorList) == 1, 
            a <- doorList, # ex: if door chosen is 1 and winning door is 3, door removed = 2
            a <- sample(doorList, 1)) # ex: if door chosen is 3 and winning door is 3, door removed is either 1 or 2 at equal odds
  return(a)
}

doorSwitching <- function(x, doorList = c(1,2,3)){ # x = row of door chosen, door win, and door removed
  doorList <- as.data.frame(doorList)
  colnames(doorList) <- c("doors")
  # examine door list and switch 
  doorList <- doorList[!(doorList$doors %in% as.list(x)),] 
  a <- doorList
  return(a)
}
doorRemoval <- function(x, doorList = c(1,2,3)){
  doorList <- as.data.frame(doorList)
  colnames(doorList) <- c("doors")
  doorList <- doorList[!(doorList$doors %in% as.list(x)),]
  ifelse(length(doorList) == 1, 
            a <- doorList, 
            a <- sample(doorList, 1))
  return(a)
}

doorSwitching <- function(x, doorList = c(1,2,3)){
  doorList <- as.data.frame(doorList)
  colnames(doorList) <- c("doors")
  doorList <- doorList[!(doorList$doors %in% as.list(x)),]
  a <- doorList
  return(a)
}
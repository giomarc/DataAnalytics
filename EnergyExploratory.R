# read csv
energydata <- read.csv(file = "EnergyEfficiencyData.csv", header = TRUE)
head(energydata)
facCol <- c("Orientation", "Glazing.Area.Distribution")
energydata[, facCol] <- lapply(energydata[,facCol], as.factor)
str(energydata)
summary(energydata)

# histogram all values are they really numerics? 
require(ggplot2)
histFun <- function(x)
{
  cnames <- names(x)
  for(i in seq_along(cnames)){
    print(ggplot(x, aes_string(x = cnames[i])) + geom_histogram())
  }
}

histFun(energydata[, -which(names(energydata) %in% facCol)])

# histogram of glazing area is this really a numeric?

# correlation scatterplot on numerics? potentially group on factors with color and shape...

# boxplot factors vs. heating load and cooling load, maybe???

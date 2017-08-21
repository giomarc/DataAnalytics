# load data

file <- "playersample.txt"
players <- read.table(file, header = TRUE, stringsAsFactors = FALSE)


# understand data structure
str(players)
head(players)
summary(players)

#remove ID column as it has no affect on analysis
require(dplyr)

players <- players %>% select(-ID)

#change numeric columns with any NAs to 0 

players[is.na(players)] <- 0

#change character column to date
players[, "INSTALLDATE"] <- as.Date(players[, "INSTALLDATE"])

#factor charater columns
players <- data.frame(lapply(players, function(x) if(is.character(x)) as.factor(x) else x))

#create a purchaser column, fails, success.rate column
players <- players %>% mutate(purchaser_30 = PURCHASES_30 > 0)
players <- players %>% mutate(fails = (ATTEMPTS + RETRYS) - COMPLETES)
players <- players %>% mutate(success.rate = ifelse(ATTEMPTS > 0, COMPLETES/(ATTEMPTS + RETRYS), 0))

#understand payer summary vs non-payers

summary(filter(players, purchaser_30 == TRUE))
summary(filter(players, purchaser_30 == FALSE))

nrow(filter(players, ATTEMPTS > 1))

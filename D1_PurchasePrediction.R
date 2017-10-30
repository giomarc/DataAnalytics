# get data

file <- "H:/R/LTV/playersample_d1.txt"
players <- read.table(file, header = TRUE, stringsAsFactors = FALSE)

str(players)
head(players)
summary(players)

require(dplyr)

players <- players %>% select(-ID)

#change numeric columns with any NAs to 0 

players[is.na(players)] <- 0

#change character column to date
players[, "INSTALLDATE"] <- as.Date(players[, "INSTALLDATE"])

#factor charater columns
players <- data.frame(lapply(players, function(x) if(is.character(x)) as.factor(x) else x))

#create a purchaser column, fails, success.rate column
players <- players %>% mutate(purchaser_30 = (PURCHASES_30-PURCHASES_1) > 0)
players <- players %>% mutate(purchaser_1 = PURCHASES_1 > 0)
players <- players %>% mutate(fails = (ATTEMPTS + RETRYS) - COMPLETES)
players <- players %>% mutate(success.rate = ifelse(ATTEMPTS > 0, COMPLETES/(ATTEMPTS), 0))

players %>% filter(LASTCOURSE >= 1) %>% count(purchaser_30)

# filter out players with last course >= 1
players.sub <- players %>% filter(LASTCOURSE >= 1) %>% select(-INSTALLDATE, -PUBLISHER, -COUNTRY, -PLATFORM)

require(rpart)
require(rpart.plot)
require(caret)

set.seed(3456)
trainIndex <- createDataPartition(players.sub$purchaser_1, p = 0.6, list = FALSE, times = 1)

players.train <- players.sub[trainIndex,]
players.test <- players.sub[-trainIndex,]

players.train <- players.train %>% select(-BOOKINGS_30, -purchaser_1, -PURCHASES_30)
players.test <- players.test %>% select(-BOOKINGS_30, -purchaser_1, -PURCHASES_30)

#SMOTE
require(DMwR)
players.train$purchaser_30 <- as.factor(players.train$purchaser_30)
players.train.smote <- SMOTE(purchaser_30 ~., players.train, k = 5, perc.over = 400, perc.under = 100)

#Train control
set.seed(2468)
seeds <- seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, )

train.ctrl <- trainControl(method = "repeatedcv", )
#Train

#Validate

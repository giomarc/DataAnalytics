# get data

file <- "H:/R/LTV/playersample_d1.txt"
players.d1 <- read.table(file, header = TRUE, stringsAsFactors = FALSE)

file <- "H:/R/LTV/playersample_d1_7.txt"
players.d7 <- read.table(file, header = TRUE, stringsAsFactors = FALSE)

players <- merge(players.d1, players.d7, by = c("ID"))

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
players <- players %>% mutate(purchaser_7 = (PURCHASES_7-PURCHASES_1) > 0)
players <- players %>% mutate(purchaser_1 = PURCHASES_1 > 0)
players <- players %>% mutate(fails = (ATTEMPTS + RETRYS) - COMPLETES)
players <- players %>% mutate(success.rate = ifelse(ATTEMPTS > 0, COMPLETES/(ATTEMPTS), 0))

players %>% filter(LASTCOURSE >= 1) %>% count(purchaser_7)

# filter out players with last course >= 1
players.sub <- players %>% filter(LASTCOURSE >= 1) %>% select(-INSTALLDATE, -PUBLISHER, -COUNTRY, -PLATFORM)

require(rpart)
require(rpart.plot)
require(caret)

set.seed(3456)
trainIndex <- createDataPartition(players.sub$purchaser_1, p = 0.6, list = FALSE, times = 1)

players.train <- players.sub[trainIndex,]
players.test <- players.sub[-trainIndex,]

players.train <- players.train %>% select(-BOOKINGS_30, -purchaser_1, -PURCHASES_30, -PURCHASES_7, -BOOKINGS_7)
players.test <- players.test %>% select(-BOOKINGS_30, -purchaser_1, -PURCHASES_30, -PURCHASES_7, -BOOKINGS_7)

####### D30 predictions ###########
#SMOTE
require(DMwR)
players.train$purchaser_30 <- as.factor(players.train$purchaser_30)
players.train.smote <- SMOTE(purchaser_30 ~., players.train, k = 5, perc.over = 400, perc.under = 100)

#Train control -> cross validation
set.seed(2468)
seeds <- seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 1)
seeds[[51]] <- sample.int(1000, 1)

test <- factor(ifelse(players.train.smote$purchaser_30 == "TRUE", "yes_d30", "no_d30"))
players.train.smote$purchaser_30 <- test

test <- factor(ifelse(players.train$purchaser_30 == "TRUE", "yes_d30", "no_d30"))
players.train$purchaser_30 <- test

# base rpart

# cross validated

train.ctrl <- trainControl(method = "repeatedcv", 
                           repeats = 5, 
                           number = 10, 
                           seeds = seeds,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)

#Train
library(doParallel)

c1 <- makeCluster(detectCores()-2)
registerDoParallel(c1)

system.time(tree.fit <- train(purchaser_30 ~., 
                  data = (players.train.smote %>% select(-LASTINVENTORY)), #!Note added d7 need to remove purchaser 7 as well if reran!!!
                  method = "rpart1SE", 
                  trControl = train.ctrl, 
                  metric = "ROC") 
)

stopCluster(c1); print("cluster stopped")
registerDoSEQ()


tree.fit
tree.fit$finalModel

rpart.plot(tree.fit$finalModel, cex = 0.75, extra = 1)

#Validate
test <- factor(ifelse(players.test$purchaser_30 == "TRUE", "yes_d30", "no_d30"))
players.test$purchaser_30 <- test

players.tree.preds <- predict(tree.fit$finalModel, (players.test %>% select(-LASTINVENTORY)))

require(pROC)
tree.roc <- roc(players.test$purchaser_30, players.tree.preds[,2])
plot(tree.roc, print.thres = TRUE)
auc(tree.roc)

threshold <- .384
pred.class <- ifelse(players.tree.preds[,2] > threshold, "yes_d30", "no_d30")
tree.cm <- confusionMatrix(pred.class, players.test$purchaser_30, positive = "yes_d30")
tree.cm

# tree with center and scale, to no avail....

preProcValues <- preProcess(players.train.smote, method = c("center", "scale"))

players.train.smote.scale <- predict(preProcValues, players.train.smote)
players.test.scale <- predict(preProcValues, players.test)

train.ctrl <- trainControl(method = "repeatedcv", 
                           repeats = 5, 
                           number = 10, 
                           seeds = seeds,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE)

tree.fit.scale <- train(purchaser_30 ~., 
                  data = players.train.smote.scale, #!Note added d7 need to remove purchaser 7 as well if reran!!!
                  method = "rpart1SE", 
                  trControl = train.ctrl, 
                  metric = "ROC") 

tree.fit.scale
tree.fit.scale$finalModel

rpart.plot(tree.fit.scale$finalModel, extra = 1)


####### D7 Predictions #######


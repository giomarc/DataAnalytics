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
players <- players %>% mutate(purchaser_30 = (PURCHASES_30-PURCHASES_3) > 0)
players <- players %>% mutate(purchaser_3 = PURCHASES_3 > 0)
players <- players %>% mutate(fails = (ATTEMPTS + RETRYS) - COMPLETES)
players <- players %>% mutate(success.rate = ifelse(ATTEMPTS > 0, COMPLETES/(ATTEMPTS), 0))

#statistics from fbasics
require(fBasics)

basicStats(players[, sapply(players, is.numeric)])[c("nobs", 
                                                     "Minimum", 
                                                     "1. Quartile", 
                                                     "Median", 
                                                     "Mean", 
                                                     "Median", 
                                                     "3. Quartile",
                                                     "Maximum",
                                                     "Stdev",
                                                     "Skewness"),]

#understand payer summary vs non-payers

summary(dplyr::filter(players, purchaser_30 == TRUE))
summary(dplyr::filter(players, purchaser_30 == FALSE))
summary(dplyr::filter(players, SESSIONS == 1))


players %>% dplyr::filter(SESSIONS == 1)  %>% count(PURCHASES_30)

# bar plots of purchasers/purchases

require(ggplot2)
require(gridExtra)

# How many purchasers are there as a proportion of players monetize after 3 days and after 30 days 

p1 <- ggplot(players, aes(x = as.factor(purchaser_3))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monetizers after 3 days", y = "Percent", x = "Monetizer")

p2 <- ggplot(players, aes(x = as.factor(purchaser_30))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monetizers after 30 days", y = "Percent", x = "Monetizer")

grid.arrange(p1, p2, ncol = 2)

# Examine 

ggplot(players) + stat_count(mapping = aes(x=purchaser_30, y=..prop.., group=1))

# Monetizers sessiontime
bw1 = (max(players$SESSIONS) - min(players$SESSIONS))/200
ggplot(players, aes(SESSIONS)) + 
  geom_histogram(binwidth = bw1) + 
  xlab('Sessions (sec.)') +
  ylab('Counts') +
  xlim(NA, 100) +
  ggtitle('Session Time first 3 days')


# plot session time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE) %>% select(SESHTIME)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE) %>% select(SESHTIME) 
plot.t(a$SESHTIME, b$SESHTIME, cols = c('First 3 days Session Time (sec.) Monetizer', 'First 3 days Session Time (sec.) Non-Monetizer'))

# plot session time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE & SESHTIME > 0) %>% select(SESHTIME)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE & SESHTIME > 0) %>% select(SESHTIME) 
plot.t(a$SESHTIME, b$SESHTIME, cols = c('First 3 days Session Time (sec.) Monetizer', 'First 3 days Session Time (sec.) Non-Monetizer'))

# plot sinks time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE & SESHTIME > 0) %>% select(TOTALSINKS)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE & SESHTIME > 0) %>% select(TOTALSINKS) 
plot.t(a$TOTALSINKS, b$TOTALSINKS, cols = c('First 3 days Sinks Monetizer', 'First 3 days Sinks Non-Monetizer'))

plot.dists.cut(a$TOTALSINKS, b$TOTALSINKS, 
               cols = c('First 3 days Sinks Monetizer', 'First 3 days Sinks Non-Monetizer'), 
               nbins = 160)

# plot last course time for 30 day purchasers

a <- players %>% dplyr::filter(purchaser_30 == TRUE & SESHTIME > 0) %>% select(LASTCOURSE)  
b <- players %>% dplyr::filter(purchaser_30 == FALSE & SESHTIME > 0) %>% select(LASTCOURSE) 
plot.dists(a$LASTCOURSE, b$LASTCOURSE, cols = c('Last Course Reached Day 3 Monetizer', 'Last Course Reached Day 3 Non-Monetizer'), nbins = 500)


plot.dists.cut(a$LASTCOURSE, b$LASTCOURSE, cols = c('Last Course Reached Day 3 Monetizer', 'Last Course Reached Day 3 Non-Monetizer'), nbins = 838)


players.sub <- players %>% dplyr::filter((SESSIONS > 1 & SESHTIME > 0) | purchaser_30 == TRUE)

players.sub <- players %>% dplyr::filter(LASTCOURSE >= 1)
summary(players.sub)

ggplot(sample_frac(players.sub, 0.10), aes(SESHTIME, TOTALSINKS)) + 
  geom_point(aes(color = factor(purchaser_30), size = BOOKINGS_30), alpha = 0.3) + 
  xlab('Session Time (sec.)') + ylab('Total Sinks') + 
  ggtitle('Engagement vs. Sinking')


ggplot(sample_frac(players.sub, 0.05), aes(x = SESHTIME, y= TOTALSINKS, fill = PURCHASES_30)) + 
  stat_binhex(aes(alpha = ..count..)) + 
  scale_alpha(name = 'Frequency', range = c(0,1)) + 
  xlab('Session Time (sec.)') + ylab('Total Sinks') + 
  ggtitle('Engagement vs. Sinking')

ggplot(sample_frac(players.sub, 0.05), aes(x = SESHTIME, y = TOTALSINKS)) +
  geom_raster(aes(fill = PURCHASES_30), interpolate = TRUE)

# density plots
colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))
p1 <- ggplot(dplyr::filter(players.sub, purchaser_30 == TRUE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 300) + xlim(0,30000) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)

p2 <- ggplot(dplyr::filter(players.sub, purchaser_30 == FALSE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 300) + xlim(0,30000) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)
# density plots with 5x zoom
p3 <- ggplot(dplyr::filter(players.sub, purchaser_30 == TRUE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 300/5) + xlim(0,30000/5) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)

p4 <- ggplot(dplyr::filter(players.sub, purchaser_30 == FALSE), aes(x = SESHTIME, y = TOTALSINKS)) +
  ylim(0, 300/5) + xlim(0,30000/5) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + 
  geom_density2d(colour="black", bins=5)

grid.arrange(p3, p4, nrow = 2)

ggplot(dplyr::filter(players.sub, purchaser_30 == TRUE), aes(x = SESHTIME, y = TOTALSINKS)) +
  geom_point(aes(color = PURCHASES_30), alpha = 0.3) +
  geom_density2d(colour="white", bins=5)

ggplot(dplyr::filter(players.sub, purchaser_30 == FALSE), aes(x = SESHTIME, y = TOTALSINKS)) +
  geom_point(alpha = 0.3) +
  geom_density2d(colour="white", bins=5)

# correlation plot

require(corrplot)

#scratch
players.sub.payers <- players.sub %>% dplyr::filter(purchaser_30 == TRUE)
players.sub.payers$payer.30.int <- as.integer(players.sub.payers$purchaser_30)
cors <- cor(players.sub.payers[, sapply(players.sub.payers, is.numeric)], method = 'pearson')

cors <- cor(cbind(players.sub[, sapply(players.sub, is.numeric)], as.integer(players.sub$purchaser_30)), 
            method = 'pearson')
#end scratch

cors <- cor(players.sub.lastcourse[, sapply(players.sub.lastcourse, is.numeric)], method = 'pearson')
corrplot.mixed(cors, upper = "ellipse", tl.cex = 0.8)

source('H:/R/DataAnalytics/cormtest.R', echo=TRUE)
corm <- cor.mtest.2(cors)


# add cor.mtest for p.value matrix.... look in help
cex.before <- par("cex")
par(cex = 0.7)
corrplot(cors,  p.mat = corm[[1]], insig = "blank", method = "color",
         addCoef.col="grey", 
         order = "AOE", tl.cex = 0.8,
         cl.cex = 1/par("cex"), addCoefasPercent = FALSE)
par(cex = cex.before)

# violin plots - session time, sessions, 
ggplot(players.sub, aes(x = factor(purchaser_30), y = SESHTIME)) + 
  geom_violin(trim = TRUE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun.y = "mean", geom = "point", colour = "red") +
  xlab('Monetizer')  + ylab('Session Time (s)') +
  ggtitle('Session time and 30 day Monetizer')

# violin plots - session time, sessions, 
ggplot(players.sub, aes(x = factor(purchaser_30), y = TOTALSINKS)) + 
  geom_violin(trim = TRUE, draw_quantiles = c(0.5, 0.75))  + 
  stat_summary(fun.y = "mean", geom = "point", colour = "red") +
  xlab('Monetizer')  + ylab('Sinks Time') + 
  ggtitle('Sinks and 30 day Monetizer')

# Last 
ggplot(players.sub, aes(x = factor(purchaser_30), y = TOTALSINKS)) + 
  geom_violin(trim = TRUE, draw_quantiles = c(0.5, 0.75))  + 
  stat_summary(fun.y = "mean", geom = "point", colour = "red") +
  xlab('Monetizer')  + ylab('Sinks Time') + 
  ggtitle('Sinks and 30 day Monetizer')

# violin plots - session time, sessions, 
ggplot(players.sub, aes(x = factor(purchaser_30), y = COMPLETES)) + 
  geom_violin(trim = TRUE, draw_quantiles = c(0.75)) +
  stat_summary(fun.y = "mean", geom = "point", colour = "red") +
  xlab('Monetizer')  + ylab('Completes') +
  ggtitle('Completes and 30 day Monetizer')

# violin plots - session time, sessions, 

aggpurch <- aggregate(data = players.sub, PURCHASES_30 ~ purchaser_3 + purchaser_30, mean)
bw1 = (max(players.sub$PURCHASES_30/10) - min(players.sub$PURCHASES_30))/30
ggplot(players.sub %>% dplyr::filter(purchaser_30 == TRUE | purchaser_3 == TRUE), aes(PURCHASES_30)) + 
  geom_histogram(binwidth = bw1) + geom_vline(aes(xintercept = aggpurch[,3]), data = aggpurch, color = "red") +
  facet_grid(purchaser_30~purchaser_3) +
  xlab('Purchases')  + ylab('Count') +
  xlim(0, 75) + 
  ggtitle('3 day and 30 day Monetizer and purchase volume')

ggplot(players.sub %>% dplyr::filter(purchaser_30 == TRUE | purchaser_3 == TRUE), aes(PURCHASES_30)) + 
  geom_histogram(binwidth = bw1) + geom_vline(aes(xintercept = aggpurch[,3]), data = aggpurch, color = "red") +
  facet_grid(purchaser_30~purchaser_3) +
  xlab('Purchases')  + ylab('Count') +
  xlim(0, 75) + 
  ggtitle('3 day and 30 day Monetizer and purchase volume')

##### prepare for linear regressions #####


# scale data
nums <- sapply(players.sub, is.numeric)
players.sub.scale <- players.sub
players.sub.scale[, nums] <- lapply(players.sub[, nums], scale)
players.sub.scale$PURCHASES_30 <- players.sub$PURCHASES_30


# remove columns not in model

players.sub.scale.model <- players.sub.scale %>% select(-INSTALLDATE, -PUBLISHER, -COUNTRY, 
                                                        -PLATFORM, -BOOKINGS_30, -purchaser_3, 
                                                        -purchaser_30)
str(players.sub.scale.model)

# split data into a training set and testing set
set.seed(369)
row.samp <- sample(1:nrow(players.sub.scale.model), 0.7*nrow(players.sub.scale.model)) # should evaluate sample weight
players.train <- players.sub.scale.model[row.samp,]
players.test <- players.sub.scale.model[-row.samp,]

#### regression ####

## kitchen sync
players.base.lm <- lm(scale(PURCHASES_30) ~ ., data = players.train)
summary(players.base.lm)
plot(players.base.lm)
AIC(players.base.lm)
BIC(players.base.lm)

coef(players.base.lm)

player.base.pred <- predict(players.base.lm, players.test)
plot.hist.single(player.base.pred, mins = min(player.base.pred), maxs = max(player.base.pred), cols = 'Baseline lin reg Preds.',nbins = 45)

actuals_preds.lm <- data.frame(cbind(actuals = players.test$PURCHASES_30, predicteds = player.base.pred))
cor_acc.lm <- cor(actuals_preds.lm)
mse.lm <- mean((abs(actuals_preds.lm$predicteds - actuals_preds.lm$actuals))^2)

base.eval <- lm.evals(p = actuals_preds.lm$predicteds, a = actuals_preds.lm$actuals)
base.eval

## Step wise 
require(MASS)
players.step.lm <- stepAIC(players.base.lm, direction = "both")
players.step.lm$anova
plot(players.step.lm)

coef(players.step.lm)


player.step.pred <- predict(players.step.lm, players.test)

actuals_preds.st <- data.frame(cbind(actuals = players.test$PURCHASES_30, predicteds = player.step.pred))
cor_acc.st <- cor(actuals_preds.st)
mse.st <- mean((abs(actuals_preds.st$predicteds - actuals_preds.st$actuals))^2)

step.eval <- lm.evals(p = actuals_preds.st$predicteds, a = actuals_preds.st$actuals)
step.eval

## Ridge/Lasso
require(glmnet)

# set model matrix
mod.players <- model.matrix(PURCHASES_30 ~. -1, data = players.train)
mod.players.test <- model.matrix(PURCHASES_30 ~. -1, data = players.test)
head(mod.players)

# glmnet - elastic net -ridge lasso
b <- players.train$PURCHASES_30
players.ridge.lasso <- glmnet(mod.players, scale(b), family = 'gaussian', nlambda = 20, alpha = 0.5)

plot(players.ridge.lasso, xvar = 'lambda', label = TRUE)
plot(players.ridge.lasso, xvar = 'dev', label = TRUE)

players.eln  <- data.frame(actual = players.test$PURCHASES_30)
ev <- glm.lin.evals(model = players.ridge.lasso, a = players.eln$actual, mod.m = mod.players.test)

plot(ev$nsme, main="Errors of Elastic Net", ylab = "Normalized Mean Square Error")

coef(players.ridge.lasso)[,17]

players.eln$score <- predict(players.ridge.lasso, newx = mod.players.test)[,17]
players.eln$resids <- players.eln$score - players.eln$actual

plot.svd.reg(players.eln, k = players.ridge.lasso$df[17])

# glmnet - logit
b <- ifelse(players.train$PURCHASES_30 > 0, 1, 0)
players.logit <- glmnet(mod.players, b, family = 'binomial', nlambda = 20, alpha = 0.5)

plot(players.logit, xvar = 'lambda', label = TRUE)
plot(players.logit, xvar = 'dev', label = TRUE)

players.eln.logit  <- data.frame(actual = ifelse(players.test$PURCHASES_30 > 0, 1, 0))
ev.l <- glm.logit.eval(model = players.logit, a = players.eln.logit$actual, mod.m = mod.players.test)

players.eln.logit$score <- predict(players.logit, newx = mod.players.test)[,10]
players.eln.logit$score <- players.eln.logit$score <- exp(players.eln.logit$score)/(1 + exp(players.eln.logit$score))
players.eln.logit$class <- ifelse(players.eln.logit$score > 0.5, 1, 0)

require(pROC)
require(caret)

l.pRoc <- roc(actual ~ score, data = players.eln.logit)
plot(l.pRoc)
confusionMatrix(players.eln.logit$class, players.eln.logit$actual, positive = 1)

# Cross validation k-folds cv
players.logit.cv <- cv.glmnet(mod.players, b, nfolds = 5, family = 'binomial', nlambda = 20, alpha = 0.5)

plot(players.logit.cv)
coef(players.logit.cv)

players.eln.logit.cv  <- data.frame(actual = ifelse(players.test$PURCHASES_30 > 0, 1, 0))
players.eln.logit.cv$score <- predict(players.logit.cv, newx = mod.players.test)
players.eln.logit.cv$score <- players.eln.logit.cv$score <- exp(players.eln.logit.cv$score)/(1 + exp(players.eln.logit.cv$score))
players.eln.logit.cv$class <- ifelse(players.eln.logit.cv$score > 0.5, 1, 0)

confusionMatrix(players.eln.logit.cv$class, players.eln.logit.cv$actual)

# non-payer capture
payer.pred <- data.frame(actual = players.eln.logit.cv$actual)
payer.pred$class.3 <-  ifelse(players.test$PURCHASES_3 > 0, 1, 0)
payer.pred$class.30 <- players.eln.logit.cv$class

table(payer.pred)

# SMOTE'd with glmnet, logit
SMOTE.train <- players.train 
SMOTE.train$purchaser_30 <- factor(ifelse(SMOTE.train$PURCHASES_30 > 0, "yes", "no"))
SMOTE.train <- SMOTE.train %>% dplyr::select(-PURCHASES_30)
SMOTE.train <- SMOTE(purchaser_30 ~., sample_frac(SMOTE.train,0.2), k = 3, perc.over = 400, perc.under = 100)

SMOTE.mod.train <- model.matrix(purchaser_30 ~. -1, data = SMOTE.train)

SMOTE.test <- players.test
SMOTE.test$purchaser_30 <- factor(ifelse(SMOTE.test$PURCHASES_30 > 0, "yes", "no"))
SMOTE.test <- SMOTE.test %>% dplyr::select(-PURCHASES_30)
SMOTE.mod.test <- model.matrix(purchaser_30 ~. -1, data = SMOTE.test)

SMOTE.logit.cv <- cv.glmnet(SMOTE.mod.train, SMOTE.train$purchaser_30, nfolds = 5, family = 'binomial', nlambda = 20, alpha = 0.5)
plot(SMOTE.logit.cv)
coef(SMOTE.logit.cv)

SMOTE.r  <- data.frame(actual = factor(ifelse(players.test$PURCHASES_30 > 0, "yes", "no")))
SMOTE.r$score <- predict(SMOTE.logit.cv, newx = SMOTE.mod.test)
SMOTE.r$score <- SMOTE.r$score <- exp(SMOTE.r$score)/(1 + exp(SMOTE.r$score))
SMOTE.r$class <- factor(ifelse(SMOTE.r$score > 0.5, "yes", "no"))

confusionMatrix(SMOTE.r$class, SMOTE.r$actual)

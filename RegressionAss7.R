# get data
read.auto = function(file = 'Automobile price data _Raw_.csv'){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  
  ## Coerce some character columns to numeric
  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price[complete.cases(auto.price), ]
}
auto.price <- read.auto()

str(auto.price)

# remove symbolizing and normalized losses
library(dplyr)
auto.price <- auto.price %>% dplyr::select(-normalized.losses,-symboling)

# scale numerical and int columns
auto.price.scaled <- data.frame(lapply(auto.price, function(x) if (is(x, "numeric")) scale(x) else x))

str(auto.price.scaled)

# create model matrix for data
mod.auto <- model.matrix(price ~. -1, data = auto.price.scaled)
head(mod.auto)


#### StepAIC ####
library(MASS)

lm.auto <- lm(price ~ ., data = auto.price.scaled)
summary(lm.auto)
plot(lm.auto)

lm.auto.step <- stepAIC(lm.auto, direction = "both")
lm.auto.step$anova
summary(lm.auto.step)
plot(lm.auto.step)

# try with price un.scaled, keep

auto.price.scaled$price.real <- auto.price$price
auto.price.scaled$log.price <- log(auto.price$price)

lm.auto <- lm(price.real ~ ., data = auto.price.scaled %>% dplyr::select(-log.price, -price))
summary(lm.auto)
plot(lm.auto)

lm.auto.step <- stepAIC(lm.auto, direction = "both")
lm.auto.step$anova
summary(lm.auto.step)
plot(lm.auto.step)


#### SVD ####
mod.auto <- model.matrix(price ~. -1, data = auto.price.scaled %>% dplyr::select(-log.price, -price.real))
head(mod.auto)

svd.auto <- svd(mod.auto)
svd.auto$d

plot(log(svd.auto$d), main = "Log singular values vs. Index")

cat('The inverse matrix of sigular values')
dtrim <- rep(0, length(svd.auto$d))
dtrim[1:57] <- 1/svd.auto$d[1:57]
dM <- diag(dtrim)
cat('The pseudo inverse of the matrix')
InvM <- svd.auto$v %*% dM %*% t(svd.auto$u)
cat('The pseudo inverse times the matrix')
invMM <- InvM %*% mod.auto

bM <- InvM %*% auto.price.scaled$price

# Note to self: 
# change this to a separate results df adding in scaled price for compatibility with plot.svd.reg

auto.results.svd <- data.frame(actual = auto.price.scaled$price)
auto.results.svd$score <- mod.auto %*% bM + mean(auto.price.scaled$price)
auto.results.svd$resids <- auto.results.svd$score - auto.results.svd$actual

## remove
auto.price.scaled$score <- mod.auto %*% bM + mean(auto.price.scaled$price)
auto.price.scaled$resids <- auto.price.scaled$score - auto.price.scaled$price

auto.price.scaled$score.rescale <- auto.price.scaled$score*sd(auto.price.scaled$price.real) + mean(auto.price.scaled$price.real)
auto.price.scaled$resids.rescale <- auto.price.scaled$score.rescale - auto.price.scaled$price.real
#remove


require(repr)
options(repr.pmales.extlot.width=8, repr.plot.height=4)

plot.svd.reg <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$actual)
  SST <- sum((df$actual - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

plot.svd.reg(auto.results.svd, k = 60)


#test plotting rescale
plot.svd.reg.rescale <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score.rescale, resids.rescale), size = 2) + 
    stat_smooth(aes(score.rescale, resids.rescale)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids.rescale)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids.rescale)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids.rescale - mean(df$resids.rescale))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score.rescale, std.resids), size = 2) + 
    stat_smooth(aes(score.rescale, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$price.real)
  SST <- sum((df$price.real - Ybar)^2)
  SSR <- sum(df$resids.rescale * df$resids.rescale)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

plot.svd.reg.rescale(auto.price.scaled, k = 60)


#### Elastic Net #####
require(glmnet)

b <- auto.price.scaled$log.price

auto.ridge.lasso <- glmnet(mod.auto, b, family = 'gaussian', nlambda = 20, alpha = 0.5)
plot(auto.ridge.lasso, xvar = 'lambda', label = TRUE)
plot(auto.ridge.lasso, xvar = 'dev', label = TRUE)

auto.results.eln  <- data.frame(actual = b)
auto.results.eln$score <- predict(auto.ridge.lasso, newx = mod.auto)[,14]
auto.results.eln$resids <- auto.results.eln$score - auto.results.eln$actual

plot.svd.reg(auto.results.eln, k = auto.ridge.lasso$df[14])

#### Reference ####
require(HistData)
require(dplyr)
Galton.scaled = GaltonFamilies[, c('mother', 'father', 'childHeight', 'gender')]
Galton.scaled = mutate(Galton.scaled, 
                       mother.sqr = mother^2, father.sqr = father^2)
Galton.scaled[, c('mother', 'father', 'mother.sqr', 'father.sqr')] = 
  lapply(Galton.scaled[, c('mother', 'father', 'mother.sqr', 'father.sqr')], 
         scale)
str(Galton.scaled)

lm.galton<- lm(childHeight ~ ., data = Galton.scaled)
summary(lm.galton)

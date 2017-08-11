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

# try with price un.scaled

auto.price.scaled$price.real <- auto.price$price
auto.price.scaled$log.price <- log(auto.price$price)

lm.auto <- lm(price.real ~ ., data = auto.price.scaled %>% dplyr::select(-log.price, -price))
summary(lm.auto)
plot(lm.auto)

lm.auto.step <- stepAIC(lm.auto, direction = "both")
lm.auto.step$anova
summary(lm.auto.step)
plot(lm.auto.step)


#### For reference

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

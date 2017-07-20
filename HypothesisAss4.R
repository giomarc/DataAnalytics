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
auto.price = read.auto()

auto.price$log.price <- log(auto.price$price) # add log price

str(auto.price)

# qqnorm on price and log price

# Visual test of normality
par(mfrow = c(1, 2))

qqnorm(auto.price$price, main = 'Q-Q plot of Price')
qqline(auto.price$price)

qqnorm(auto.price$log.price, main = 'Q-Q plot of Log Price')
qqline(auto.price$log.price)
par(mfrow = c(1, 1))

plot(sort(auto.price$price), sort(auto.price$log.price), main = 'Plot of Price vs. Log Price', 
     xlab = 'Quantiles of Price', ylab = 'Qunatiles of Log Price')
abline(a = 0.0, b = 1.0, lty = 2, col = 'blue')


#keep
par(mfrow = c(1, 2))

qqnorm(scale(auto.price$price), main = 'Q-Q plot of Price')
qqline(scale(auto.price$price))

qqnorm(scale(auto.price$log.price), main = 'Q-Q plot of Log Price')
qqline(scale(auto.price$log.price))

par(mfrow = c(1, 1))

plot(sort(scale(auto.price$price)), sort(scale(auto.price$log.price)), main = 'Plot of Standardized Price vs. Log Price', 
     xlab = 'Quantiles of Std. Price', ylab = 'Qunatiles of Std. Log Price')
abline(a = 0.0, b = 1.0, lty = 2, col = 'blue')




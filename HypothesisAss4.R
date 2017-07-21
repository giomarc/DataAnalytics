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

#In Report:

# Q-Q plots 
par(mfrow = c(1, 2))

qqnorm(scale(auto.price$price), main = 'Q-Q plot of Price')
qqline(scale(auto.price$price))

qqnorm(scale(auto.price$log.price), main = 'Q-Q plot of Log Price')
qqline(scale(auto.price$log.price))

par(mfrow = c(1, 1))

plot(sort(scale(auto.price$price)), sort(scale(auto.price$log.price)), main = 'Plot of Standardized Price vs. Log Price', 
     xlab = 'Quantiles of Std. Price', ylab = 'Qunatiles of Std. Log Price')
abline(a = 0.0, b = 1.0, lty = 2, col = 'blue')

#cumalitive density
x_seq <- seq(-3,3,len=length(auto.price$price))
y_cdf1 <- sapply(x_seq, function(x){
  sum(scale(auto.price$price)<x)/length(auto.price$price)
})
y_cdf2 <- sapply(x_seq, function(x){
  sum(scale(auto.price$log.price)<x)/length(auto.price$log.price)
})

plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized car prices', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16) 
legend("bottomright", legend = c("price", "log(price)"), col = c("blue", "red"), cex = 0.8)

## Find the max deviation
k_s_stat <- max(abs(y_cdf1 - y_cdf2))
k_s_stat
# where does it occur?
k_index = which.max(abs(y_cdf1-y_cdf2))
k_s_x = x_seq[k_index]
plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16) 
lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
      col='black', lwd=8)

# IN REPORT

# Create k-s statistic function
ks_stat <- function(x_min,x_max, dist_a, dist_b, makeplot = FALSE, labels = c("Price","Normal Dist.")){
  x_seq <- seq(x_min,x_max,len=length(dist_a))
  y_cdf1 <- sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 <- sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat <- max(abs(y_cdf1-y_cdf2))
  
  if (makeplot == TRUE){
    k_index <- which.max(abs(y_cdf1-y_cdf2))
    k_s_x <- x_seq[k_index]
    plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized car prices', 
         xlab = 'Value', ylab = 'Cumulative density')
    points(x_seq,y_cdf2,col='red', pch=16) 
    lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
          col='black', lwd=8)
    legend("bottomright", legend = c(labels, c("Max diff")), col = c("blue", "red", "black"),lty = 1, cex = 0.8)
  }
  return(k_s_stat)
}

par(mfrow = c(1, 2))

dist_a <- scale(auto.price$price)
dist_b <- rnorm(length(auto.price$price), 0, 1)
ks_stat(-3, 3, dist_a, dist_b, makeplot = TRUE, labels = c("Car Price", "Normal"))

dist_a <- scale(auto.price$log.price)
dist_b <- rnorm(length(auto.price$log.price), 0, 1)
ks_stat(-3, 3, dist_a, dist_b, makeplot = TRUE, labels = c("log(Car Price)", "Normal"))

par(mfrow = c(1, 1))

##----Repeat N Times-----
N <- 1000
k_s_rep <- sapply(1:N, function(i){
  dist_a <- scale(auto.price$log.price)
  dist_b <- rnorm(length(auto.price$log.price), 0, 1)
  return(ks_stat(-3, 3, dist_a, dist_b))
})

hist(k_s_rep, breaks=30, freq=FALSE, xlab = 'K-S statistic',
     main = 'Histogram of k-s statistic log Car Price')
lines(density(k_s_rep))
abline(v = mean(k_s_rep), col = c("red"), lty = 2)
legend("topright", legend = c("Mean KS"), col = c("red"),lty = 1, cex = 0.8)

## KS Tests
ks.test(scale(auto.price$price), rnorm(length(auto.price$price), 0, 1), alternative = "two.sided")
ks.test(scale(auto.price$log.price), rnorm(length(auto.price$log.price), 0, 1), alternative = "two.sided")

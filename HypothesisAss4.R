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

#End in report

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

#end of in report

# t-tests
require(broom)
require(dplyr)
require(HistData)
auto.price %>% group_by(aspiration) %>% do(tidy(t.test(log.price~aspiration, data = auto.price)))

#in Report:

# test power of differences and sample sizes
table(auto.price$aspiration)
table(auto.price$fuel.type)
table(auto.price$drive.wheels)


require(pwr)

X = seq(from = 0.0, to = 1.5, length.out = 100)
powers = sapply(X, function(x) 
  pwr.t.test(n = 20, d = x, sig.level = 0.05, power = NULL,
             type = "two.sample", alternative = "two.sided")$power)

plot(X, powers, type = 'l', lwd = 2, col = 'red',
     xlab = 'Difference of means', ylab = 'Power',
     main = 'Power vs. difference of means')

X = seq(from = 1, to = 200, length.out = 100)
powers = sapply(X, function(x) 
  pwr.t.test(n = x, d = 0.5, sig.level = 0.05, power = NULL,
             type = "two.sample", alternative = "two.sided")$power)

plot(X, powers, type = 'l', lwd = 2, col = 'red',
     xlab = 'Sample size', ylab = 'Power',
     main = 'Power vs. sample size')

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 20){
  maxs = max(c(max(a), max(b)))
  mins = min(c(min(a), min(b)))
  breaks = seq(maxs, mins, length.out = (nbins + 1))
  par(mfrow = c(2, 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
  abline(v = mean(a), lwd = 4, col = 'red')
  hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
  abline(v = mean(b), lwd = 4, col = 'red')
  par(mfrow = c(1, 1))
}

plot.power.mean <- function(n1 = 100, n2 =100, sig.level =0.05, max.mean = 0.5){
  X <- seq(from = 0.0, to = max.mean, length.out = 100)
  powers <- sapply(X, function(x) 
    pwr.t2n.test(n1 = n1, n2 = n2, d = x, sig.level = sig.level, power = NULL,
               alternative = "two.sided")$power)
  
  plot(X, powers, type = 'l', lwd = 2, col = 'red',
       xlab = 'Mean Diff', ylab = 'Power',
       main = 'Mean Diff vs. Power')
}

table(auto.price$aspiration)
t.test(log.price~aspiration, data = auto.price)
plot.t(auto.price$log.price[auto.price$aspiration == "std"], 
       auto.price$log.price[auto.price$aspiration == "turbo"],
       cols = c("std", "turbo"))

pwr.t2n.test(n1 = length(auto.price$log.price[auto.price$aspiration == "std"]), 
             n2 = length(auto.price$log.price[auto.price$aspiration == "turbo"]), 
             d = mean(auto.price$log.price[auto.price$aspiration == "std"]) - mean(auto.price$log.price[auto.price$aspiration == "turbo"]), 
             sig.level = 0.05, power = NULL,
             alternative = "two.sided")

plot.power.mean(n1 = length(auto.price$log.price[auto.price$aspiration == "std"]),
                n2 = length(auto.price$log.price[auto.price$aspiration == "turbo"]))

table(auto.price$fuel.type)
t.test(log.price~fuel.type, data = auto.price)
plot.t(auto.price$log.price[auto.price$fuel.type == "diesel"], 
       auto.price$log.price[auto.price$fuel.type == "gas"],
       cols = c("diesel", "gas"))

pwr.t2n.test(n1 = length(auto.price$log.price[auto.price$fuel.type == "diesel"]), 
             n2 = length(auto.price$log.price[auto.price$fuel.type == "gas"]), 
             d = abs(mean(auto.price$log.price[auto.price$fuel.type == "diesel"]) - mean(auto.price$log.price[auto.price$fuel.type == "gas"])), 
             sig.level = 0.05, power = NULL,
             alternative = "two.sided")

plot.power.mean(n1 = length(auto.price$log.price[auto.price$fuel.type == "diesel"]),
                n2 = length(auto.price$log.price[auto.price$fuel.type == "gas"]))

table(auto.price$drive.wheels)
t.test(log.price~drive.wheels, data = auto.price[auto.price$drive.wheels != "4wd",])
plot.t(auto.price$log.price[auto.price$drive.wheels == "fwd"], 
       auto.price$log.price[auto.price$drive.wheels == "rwd"],
       cols = c("fwd", "rwd"))

pwr.t2n.test(n1 = length(auto.price$log.price[auto.price$drive.wheels == "fwd"]), 
             n2 = length(auto.price$log.price[auto.price$drive.wheels == "rwd"]), 
             d = abs(mean(auto.price$log.price[auto.price$drive.wheels == "fwd"]) - mean(auto.price$log.price[auto.price$drive.wheels == "rwd"])), 
             sig.level = 0.05, power = NULL,
             alternative = "two.sided")

plot.power.mean(n1 = length(auto.price$log.price[auto.price$drive.wheels == "fwd"]),
                n2 = length(auto.price$log.price[auto.price$drive.wheels == "rwd"]))

#end of in report.



#ANOVA

table(auto.price$body.style)
aggregate(auto.price$log.price, list(auto.price$body.style), mean)
plot.power.mean(n1 = length(auto.price$log.price[auto.price$body.style == "convertible"]),
                n2 = length(auto.price$log.price[auto.price$body.style == "hardtop"]), max.mean = 1.0)

# remove convertible and hardtop

auto.price$body.style <- factor(auto.price$body.style)

auto.price.sub <- auto.price[!(auto.price$body.style) %in% c("convertible", "hardtop"),]

auto.price.sub$body.style <- factor(auto.price.sub$body.style)
boxplot(auto.price.sub$log.price ~ auto.price.sub$body.style, main = "Box plots body styles")

auto.sub_aov = aov(log.price ~ body.style, data = auto.price.sub)
summary(auto.sub_aov)
print(auto.sub_aov)

tukey_anova = TukeyHSD(auto.sub_aov)  # Tukey's Range test:
tukey_anova

plot(tukey_anova)

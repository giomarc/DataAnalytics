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

# Helper Graphical Functions
plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
  breaks <- seq(maxs, mins, length.out = (nbins + 1))
  hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
  abline(v = mean(a), lwd = 4, col = 'red')
  abline(v = 0, lwd = 4, col = 'blue')
  abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
  abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 80, p = 0.05){
  maxs <- max(c(max(a), max(b)))
  mins <- min(c(min(a), min(b)))
  par(mfrow = c(2, 1))
  plot.hist(a, maxs, mins, cols = cols[1])
  plot.hist(b, maxs, mins, cols = cols[2])
  par(mfrow = c(1, 1))
}

## Bootstrap the difference in means of sons and daughters
plot.diff <- function(a, cols = 'difference of means', nbins = 80, p = 0.05){
  maxs <- max(a)
  mins <- min(a)
  plot.hist(a, maxs, mins, cols = cols[1])
}

require(dplyr)
require(resample)
require(simpleboot)
require(pwr)

# log price vs. aspiration

auto.turbo <- filter(auto.price, aspiration == "turbo")
auto.standard <- filter(auto.price, aspiration == "std")

# Bootstrap the mean of turbos and standard aspiration
mean.boot.turbo <- one.boot(auto.turbo$log.price, mean, R = 100000)
mean.boot.std <- one.boot(auto.standard$log.price, mean, R = 100000)
plot.t(mean.boot.turbo$t, mean.boot.std$t, 
       cols = c("Log Price:Aspiration-Turbo", "Log Price: Aspiration-Std"), nbins = 80)

# Bootstrap the mean difference of turbos and standard apsiration
two.boot.mean.asp <- two.boot(auto.turbo$log.price, auto.standard$log.price, mean, R = 100000)
plot.diff(two.boot.mean.asp$t, cols = 'Mean Difference Log Price ~ Aspiration(turbo-std)')


# Verfiy Normal
qqnorm(two.boot.mean.asp$t, main = 'Quantiles of standard Normal vs. \ 
       bootstrapped mean of LogPrice~Asp')

# Welch's t-test
t.test(mean.boot.turbo$t, mean.boot.std$t)

# Compare power from last week
pwr.t2n.test(n1 = length(mean.boot.turbo$t), 
             n2 = length(mean.boot.std$t), 
             d = mean(mean.boot.turbo$t) - mean(mean.boot.std$t), 
             sig.level = 0.05, power = NULL,
             alternative = "two.sided")

# log price vs. fuel type

auto.gas <- filter(auto.price, fuel.type == "gas")
auto.diesel <- filter(auto.price, fuel.type == "diesel")

# Bootstrap the mean of turbos and standard aspiration
mean.boot.gas <- one.boot(auto.gas$log.price, mean, R = 100000)
mean.boot.diesel <- one.boot(auto.diesel$log.price, mean, R = 100000)
plot.t(mean.boot.gas$t, mean.boot.diesel$t, 
       cols = c("Log Price: Fuel-Gas", "Log Price: Fuel-Diesel"), nbins = 80)

# Bootstrap the mean difference of turbos and standard apsiration
two.boot.mean.fuel <- two.boot(auto.gas$log.price, auto.diesel$log.price, t.test, R = 100000)
plot.diff(two.boot.mean.fuel$t, cols = 'Mean Difference Log Price ~ fuel type(gas-diesel)')

# Verfiy Normal
qqnorm(two.boot.mean.fuel$t, main = 'Quantiles of standard Normal vs. \ 
       bootstrapped mean of LogPrice~Asp')

# Welch's t-test
t.test(mean.boot.gas$t, mean.boot.diesel$t)


# Compare power from last week
pwr.t2n.test(n1 = length(mean.boot.gas$t), 
             n2 = length(mean.boot.diesel$t), 
             d = mean(mean.boot.gas$t) - mean(mean.boot.diesel$t), 
             sig.level = 0.05, power = NULL,
             alternative = "two.sided")

g <- boot.comb(a = auto.price, f = auto.price$body.style, n =100)



# test code
i <- length(split.a)

ml <- matrix(0, ncol = i-1, nrow = i-1)
mcol <- 1
mrow <- 1
for(l in 1:ncol(y)){
  ml[mrow, mcol] <- l
  if (l != ncol(y)){
    if(y[1, l] != y[1,l+1]){
       mcol <- 1
       mrow <- mrow + 1 }
    else {
       mcol <- mcol + 1
    }
  }
}

split.a <- split(auto.price, auto.price$body.style)
split.a.boot <- data.frame(matrix(ncol = 0, nrow = 100))

for(i in 1:5){
  split.a.boot <- cbind(split.a.boot, one.boot(split.a[[i]]$log.price, mean, R = 100)$t)
}